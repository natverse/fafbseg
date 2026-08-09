#' Return neuron metadata from Cambridge seatables
#'
#' @description This function is a generic building block for access to
#'   experimental/in progress neuron metadata. It is intended for internal use
#'   and the end user or developer is responsible for choosing the active CAVE
#'   dataset (see \code{\link{choose_segmentation}}).
#'
#' @param ids Root ids (as character or int64 vector) or a query (see examples)
#' @param ignore.case for queries whether to ignore the case
#' @param fixed whether to treat queries as a fixed string
#' @param table The name of the table to query
#' @param base Optional name of the seatable base containing the table
#'   (sometimes the table may not be found or two bases contain a table with the
#'   same name).
#' @param unique Whether to drop rows that have the same root_id. See details.
#'   There is no special logic in choosing which rows to drop, but the dropped
#'   rows are retained as an attribute on the table with a warning so that you
#'   can inspect.
#' @param translate_ids Whether to bring explicitly supplied \code{ids} forward
#'   to the requested \code{version}/\code{timestamp} before matching. \code{NA}
#'   (the default) turns this on automatically when a \code{version} or
#'   \code{timestamp} is given (see details).
#' @param token Optional API token. When supplied, the \code{FLYTABLE_TOKEN}
#'   environment variable is temporarily set to this value for the duration of
#'   the call (and restored on exit) so you can authenticate against an
#'   alternative seatable instance without permanently overwriting your token.
#'   Typically used in combination with the \code{fafbseg.flytable.url}
#'   \link[=fafbseg-package]{package option}.
#' @param drop_status Character vector of \code{status} tokens whose rows are
#'   dropped before any query/join/\code{unique} step. Matching is
#'   case-insensitive and token-wise, so it also handles multi-select
#'   \code{status} columns holding comma-separated tokens (e.g. CRANT's
#'   capitalised \code{DUPLICATED}). Pass \code{NULL} or \code{character(0)} to
#'   keep every row.
#' @param ... Additional arguments passed to \code{\link{flytable_cached_table}}
#'   (e.g. \code{expiry}, \code{refresh}) which can be used to control details
#'   of the cache strategy.
#' @inheritParams flywire_timestamp
#' @details This function now uses \code{\link{flytable_cached_table}} for
#'   efficient row-wise caching of metadata. The defaults should be a good
#'   trade-off between cache speed and getting the latest updates, but you can
#'   set \code{expiry = 0} if you want to ensure that you are as up to date as
#'   possible - this still only downloads new changes and is very fast (300ms vs
#'   100ms for a pre-cached dataset with 14k rows).
#'
#'   Note that rows whose `status` matches `drop_status` (by default
#'   `duplicate` or `bad_nucleus`) are dropped even before the `unique` argument
#'   is processed. Matching is case-insensitive and token-wise, so it works for
#'   multi-select `status` columns holding comma-separated tokens.
#'
#'   When \code{version} or \code{timestamp} are specified the table's root ids
#'   are brought to that timepoint via the \code{supervoxel_id} column. For a
#'   query string the match then happens against that mapped table, so no
#'   further work is needed. For explicit root \code{ids} the join is by
#'   \code{root_id}, so ids that are stale relative to the requested timepoint
#'   would silently fail to match. \code{translate_ids} guards against this by
#'   bringing only the unmatched ids forward with \code{\link{flywire_latestid}}
#'   (ids already present in the mapped table need no work, and
#'   \code{flywire_latestid} only does a supervoxel lookup for genuinely
#'   outdated ids). The default (\code{NA}) enables it whenever a
#'   \code{version}/\code{timestamp} is supplied; with neither, nothing is
#'   translated since the table is simply at the state of its last update.
#'
#'   If \code{translate_ids} is forced \code{TRUE} with no
#'   \code{version}/\code{timestamp}, ids are aligned to the table's own sync
#'   time (its \code{mtime} attribute) rather than live 'now', so both sides
#'   share a clock.
#'
#' @returns A data frame with appropriate rows based on the \code{ids} argument.
#'
#' @export
#'
#' @examples
#' # implies type
#' \dontrun{
#' cam_meta("MBON.+")
#' cam_meta("class:ALPN")
#' # ensure that root ids match the most recent materialisation
#' cam_meta("class:ALPN", version='latest')
#'
#' with_aedes(cam_meta)
#'
#' }
cam_meta <- function(ids=NULL, ignore.case = F, fixed = F, table='aedes_main',
                     base=NULL,
                     version=NULL, timestamp=NULL, unique=FALSE,
                     translate_ids=NA, token=NULL,
                     drop_status=c("duplicate", "bad_nucleus"), ...) {

  if (!is.null(token))
    withr::local_envvar(FLYTABLE_TOKEN = token)

  if(is.character(ids) && length(ids)==1 && !valid_id(ids) && substr(ids,1,1)=="/")
    ids=substr(ids,2, nchar(ids))
  if(is.character(ids) && length(ids)==1 && !valid_id(ids) && !grepl(":", ids))
    ids=paste0("type:", ids)

  aedes_main = fafbseg::flytable_cached_table(table = table, base=base, ...)
  # capture before any dplyr verb below strips this attribute
  table_mtime = attr(aedes_main, 'mtime')
  fields=colnames(aedes_main)
  if("status" %in% fields && length(drop_status))
    aedes_main = aedes_main[!status_matches(aedes_main$status, drop_status), , drop=FALSE]

  if(is.character(ids) && length(ids)==1 && grepl(":", ids)) {
    # it's a query
    ul = unlist(strsplit(ids, ":", fixed = T))
    if (length(ul) != 2)
      stop("Unable to parse flytable id specification!")
    target = ul[1]
    if (!target %in% colnames(aedes_main))
      stop("Unknown field in flytable id specification!")
    query = ul[2]
    if(!fixed && substr(query,1,1)!="^") {
      # regex queries are always considered to be full length
      query=paste0("^", query, "$")
    }
    df=dplyr::filter(aedes_main, grepl(query, .data[[target]], ignore.case = ignore.case, fixed = fixed))
  } else if(is.null(ids))
    df=aedes_main
  else {
    ids <- fafbseg::flywire_ids(ids, integer64 = FALSE, unique = TRUE)
    if(is.na(translate_ids))
      translate_ids <- !is.null(version) || !is.null(timestamp)
    # Map the table to the requested timepoint first, so the membership test
    # below is against the root_ids the join will actually see.
    if(!is.null(version) || !is.null(timestamp))
      aedes_main$root_id=fafbseg::flywire_updateids(aedes_main$root_id, svids = aedes_main$supervoxel_id, version = version, timestamp = timestamp)
    if(isTRUE(translate_ids)) {
      tts <- timestamp
      if(is.null(version) && is.null(timestamp) && !is.null(table_mtime))
        # No timepoint pins the table, so align ids to its own sync time (mtime)
        # rather than live 'now', which the un-mapped table lags.
        tts <- flytable_parse_date(table_mtime, format = 'timestamp')
      # Only ids absent from the (mapped) table can need translating: a stale id
      # can never equal a mapped-table root. Translate just those -- and
      # flywire_latestid only does the supervoxel lookup for genuinely outdated
      # ones, so present-and-current ids cost nothing.
      needs <- !ids %in% aedes_main$root_id
      if(any(needs))
        ids[needs] <- fafbseg::flywire_latestid(ids[needs], version = version,
                                                timestamp = tts)
    }
    df=data.frame(root_id=ids)
    df=dplyr::left_join(df, aedes_main, by='root_id')
  }

  if (isTRUE(unique)) {
    dups = duplicated(df$root_id)
    ndups = sum(dups)
    if (ndups > 0) {
      dupids = unique(df$root_id[dups])
      duprows = df[df$root_id %in% dupids, , drop = F]
      duprows = duprows[order(duprows$root_id), , drop = F]
      df = df[!dups, , drop = F]
      attr(df, "duprows") = duprows
      warning("Dropping ", sum(dups), " rows containing duplicate root_ids!\n",
              "You can inspect all ", nrow(duprows), " rows with duplicate ids by doing:\n",
              "attr(df, 'duprows')\n", "on your returned data frame (replacing df as appropriate).")
    }
  }

  if(!is.null(version) || !is.null(timestamp)) {
    # `_id` (flytable PK) is non-NA for any actual flytable row; skip those
    # rows so flywire_updateids() doesn't warn for "bad supervoxel info"
    in_table <- !is.na(df[["_id"]])
    if (any(in_table)) {
      df$root_id[in_table] <- fafbseg::flywire_updateids(
        df$root_id[in_table], svids = df$supervoxel_id[in_table],
        version = version, timestamp = timestamp)
    }
  }
  df
}

# TRUE for each status value that contains any of `drop` as a token. Handles
# multi-select columns (comma-separated tokens) and is case-insensitive, so a
# single lowercase token like "duplicate" and CRANT's capitalised, comma-joined
# "DUPLICATED" both match. NA/empty status never matches.
status_matches <- function(status, drop) {
  if(!length(drop)) return(logical(length(status)))
  drop <- tolower(trimws(drop))
  toks <- strsplit(tolower(as.character(status)), ",", fixed = TRUE)
  vapply(toks, function(t) any(trimws(t) %in% drop), logical(1))
}
