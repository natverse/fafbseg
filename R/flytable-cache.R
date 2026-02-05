# flytable disk caching with delta sync based on _mtime column

#' Fetch current server time from flytable
#'
#' @param table Table name to query (used to verify connectivity)
#' @return Character timestamp from server
#' @keywords internal
flytable_now <- function(table='info') {
  res=flytable_query(paste('select now() from', table, 'limit 1'))
  if(!is.data.frame(res))
    stop("Unable to connect to flytable to get time now!")
  res[[1]]
}


#' Fetch sync metadata in a single query
#'
#' Returns server time, row count, and max modification time.
#' Uses max(now()) trick to combine now() with other aggregates.
#' @return A list with elements: now, nrow, max_mtime
#' @keywords internal
#' @noRd
flytable_sync_metadata <- function(table) {
  res <- flytable_query(
    paste('select max(now()), count(_id), max(_mtime) from', table)
  )
  if (!is.data.frame(res) || nrow(res) == 0) {
    stop("Unable to fetch sync metadata from flytable")
  }

  list(
    now = res[[1]],
    nrow = res[[2]],
    max_mtime = res[[3]]
  )
}


#' Get a complete flytable table with disk caching and delta sync
#'
#' @description Fetches a complete flytable table using disk caching with
#'   intelligent delta synchronization. On first call, downloads the full table.
#'   On subsequent calls within \code{expiry} seconds, returns cached data.
#'   After expiry, performs delta sync fetching only rows modified since last
#'   sync based on the \code{_mtime} column.
#'
#' @details The function uses the same disk cache infrastructure as
#'   \code{\link{flywire_leaves}}. The cache location can be controlled via the
#'   \code{fafbseg.cachedir} option.
#'
#'   Delta synchronization works by:
#'   \enumerate{
#'     \item Checking if cached data exists and is within the expiry window
#'     \item If expired, querying rows where \code{_mtime > cached_mtime}
#'     \item Updating modified rows, appending new rows
#'     \item Detecting and removing deleted rows via row count comparison
#'   }
#'
#'   Error handling:
#'   \itemize{
#'     \item Connection failures during sync: returns cached data with warning
#'     \item Schema changes (columns differ): forces full refresh
#'     \item Corrupted cache: clears and re-fetches
#'   }
#'
#' @param table Table name (e.g., "info", "optic", "testfruit")
#' @param expiry Seconds before checking for updates (default 300 = 5 minutes).
#'   Set to 0 to always check for updates, \code{Inf} to never check.
#' @param refresh Logical. If \code{TRUE}, forces a complete re-download
#'   ignoring any cached data. Default \code{FALSE}.
#' @param collapse_lists Logical. If \code{TRUE} (default), collapses
#'   multi-select columns into comma-separated strings. Passed to
#'   \code{\link{flytable_query}}.
#' @param base Optional base name if table name is ambiguous (exists in multiple
#'   bases).
#' @inheritParams flytable_query
#'
#' @return A \code{data.frame} containing all rows from the table. Has an
#'   \code{mtime} attribute recording the server timestamp at last sync.
#'
#' @family flytable
#' @export
#' @seealso \code{\link{flytable_query}}, \code{\link{flytable_list_rows}}
#'
#' @examples
#' \dontrun{
#' # First call - full fetch
#' info <- flytable_cached_table("info")
#'
#' # Subsequent call within 5 min - returns cached data
#' info2 <- flytable_cached_table("info")
#'
#' # Force check for updates (ignores expiry window)
#' info3 <- flytable_cached_table("info", expiry = 0)
#'
#' # Force complete re-download
#' info4 <- flytable_cached_table("info", refresh = TRUE)
#'
#' # Check when data was last synced
#' attr(info, "mtime")
#' }
flytable_cached_table <- function(table, expiry = 300, refresh = FALSE,
                                  collapse_lists = TRUE, base = NULL,
                                  limit = 100000L) {
  fc <- flytable_cache()
  cache_key <- flytable_cache_key(table, base)

  # Force refresh - clear cache and fetch fresh
  if (isTRUE(refresh)) {
    fc$remove(cache_key)
  }

  res <- fc$get(cache_key)

  # Cache miss - do full fetch
  if (cachem::is.key_missing(res) || isTRUE(refresh)) {
    res <- flytable_full_fetch(table, base = base, collapse_lists = collapse_lists,
                               limit = limit)
    if (is.null(res)) {
      stop("Failed to fetch table '", table, "' from flytable")
    }
    fc$set(cache_key, res)
    return(res)
  }

  # Cache hit - check if within expiry window
  oldmtime <- attr(res, 'mtime')
  if (is.null(oldmtime)) {
    # Corrupted cache entry without mtime - refetch
    warning("Cache entry for '", table, "' missing mtime attribute, refetching")
    fc$remove(cache_key)
    return(flytable_cached_table(table, expiry = expiry, refresh = TRUE,
                                 collapse_lists = collapse_lists, base = base,
                                 limit = limit))
  }

  # Check time since last sync
  cached_time <- flytable_parse_date(oldmtime, format = 'timestamp')
  delta <- as.numeric(difftime(Sys.time(), cached_time, units = 'secs'))

  if (delta < expiry) {
    return(res)
  }

  # Expired - attempt delta sync
  tryCatch({
    res <- flytable_delta_sync(res, table, oldmtime, base = base,
                               collapse_lists = collapse_lists, limit = limit)
    fc$set(cache_key, res)
    res
  }, error = function(e) {
    # Check if it's a schema change
    if (grepl("schema|column", e$message, ignore.case = TRUE)) {
      warning("Schema change detected for '", table, "', forcing full refresh")
      return(flytable_cached_table(table, expiry = expiry, refresh = TRUE,
                                   collapse_lists = collapse_lists, base = base,
                                   limit = limit))
    }
    # Connection or other error - return cached data with warning
    warning("Delta sync failed for '", table, "': ", conditionMessage(e),
            "\nReturning cached data from ", oldmtime)
    res
  })
}


#' Full fetch of a flytable table
#' @keywords internal
#' @noRd
flytable_full_fetch <- function(table, base = NULL, collapse_lists = TRUE,
                                limit = 100000L) {
  mtime <- tryCatch(
    flytable_now(table),
    error = function(e) {
      stop("Unable to connect to flytable: ", conditionMessage(e))
    }
  )

  res <- flytable_query(paste('select * from', table), base = base,
                        collapse_lists = collapse_lists, limit = limit)

  if (is.null(res) || !is.data.frame(res)) {
    return(NULL)
  }

  attr(res, 'mtime') <- mtime
  res
}


#' Delta sync: fetch only modified rows since last sync
#' @keywords internal
#' @noRd
flytable_delta_sync <- function(cached_data, table, oldmtime, base = NULL,
                                collapse_lists = TRUE, limit = 100000L) {
  # Get server time, row count, and max modification time
  meta <- flytable_sync_metadata(table)
  mtime <- meta$now
  n_total <- meta$nrow
  max_mtime <- meta$max_mtime

  # Parse timestamps for comparison
  cached_time <- flytable_parse_date(oldmtime, format = 'timestamp')
  server_max_time <- flytable_parse_date(max_mtime, format = 'timestamp')

  # Check if any modifications occurred since last sync
  has_modifications <- server_max_time > cached_time

  # Calculate potential deletions from row count
  n_original <- nrow(cached_data)
  n_deleted_estimate <- n_original - n_total  # Negative means additions

  res <- cached_data

  if (has_modifications) {
    # Fetch modified rows since last sync
    qq <- glue::glue("select * from {table} where datedif(`_mtime`, '{oldmtime}', 'S') < 0")
    modrows <- suppressWarnings(flytable_query(qq, base = base,
                                               collapse_lists = collapse_lists,
                                               limit = limit))

    if (!is.null(modrows) && nrow(modrows) > 0) {
      # Check for schema changes
      if (!setequal(colnames(modrows), colnames(cached_data))) {
        stop("Schema change detected: columns differ between cached and fresh data")
      }

      rowidxs <- match(modrows[["_id"]], res[["_id"]])
      isnew <- is.na(rowidxs)

      # Update existing rows
      if (!all(isnew)) {
        res[na.omit(rowidxs), ] <- modrows[!isnew, , drop = FALSE]
      }

      # Append new rows
      if (any(isnew)) {
        res <- dplyr::bind_rows(res, modrows[isnew, , drop = FALSE])
        # Adjust deletion estimate for new rows
        n_deleted_estimate <- n_deleted_estimate + sum(isnew)
      }
    }
  }

  # Remove deleted rows if count indicates deletions
  if (n_deleted_estimate > 0) {
    res <- flytable_remove_deleted(res, table)
  }

  attr(res, 'mtime') <- mtime
  res
}


#' Remove rows that have been deleted from the server
#' @keywords internal
#' @noRd
flytable_remove_deleted <- function(cached_data, table) {
  # Fetch all current IDs from server
  all_ids <- flytable_query(glue::glue('select `_id` from {table}'))

  if (is.null(all_ids) || nrow(all_ids) == 0) {
    warning("Could not fetch IDs to check for deletions")
    return(cached_data)
  }

  # Remove rows not in current server IDs
  keep <- cached_data[["_id"]] %in% all_ids[[1]]
  cached_data[keep, , drop = FALSE]
}


#' Get the flytable disk cache
#' @keywords internal
#' @noRd
flytable_cache <- function() {
  flywire_leaves_cache(subdir = 'flytable', hybrid = TRUE)
}


#' Generate a unique cache key for a flytable table
#'
#' Returns a cachem-safe key (lowercase alphanumeric): tablename + hash(url+base).
#' @keywords internal
#' @noRd
flytable_cache_key <- function(table, base = NULL,
                               url = getOption("fafbseg.flytable.url",
                                               "https://flytable.mrc-lmb.cam.ac.uk/")) {
  if (is.null(base)) {
    # Look up the table to get base name
    tdf <- flytable_alltables(cached = TRUE)
    tdf.sel <- tdf[tdf$name == table, , drop = FALSE]

    if (nrow(tdf.sel) == 0) {
      # Table not found - will fail later during fetch
      base <- ""
    } else if (nrow(tdf.sel) > 1) {
      stop("Multiple tables named '", table, "'. Please specify the 'base' argument.")
    } else {
      base <- tdf.sel$base_name[1]
    }
  }

  # Hash url+base only; table is the readable prefix
  table_clean <- tolower(gsub("[^a-z0-9]", "", table))
  hash <- digest::digest(paste(url, base, sep = "/"), algo = 'xxhash64')
  paste0(table_clean, hash)
}
