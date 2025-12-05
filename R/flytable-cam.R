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
#' @inheritParams flywire_timestamp
#' @details Note that rows with status `duplicate` or `bad_nucleus` are dropped
#'   even before the `unique` argument is processed.
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
                     version=NULL, timestamp=NULL, unique=FALSE) {

  if(is.character(ids) && length(ids)==1 && !fafbseg:::valid_id(ids) && !grepl(":", ids))
    ids=paste0("type:", ids)
  if(is.character(ids) && length(ids)==1 && !fafbseg:::valid_id(ids) && substr(ids,1,1)=="/")
    ids=substr(ids,2, nchar(ids))

  fields=cam_meta_cols(table, base=base)

  qu=glue::glue("select * from {table}")
  if("status" %in% fields) {
    qu=paste(qu, "WHERE status NOT IN ('duplicate', 'bad_nucleus')")
  }
  aedes_main=fafbseg::flytable_query(qu, base = base)
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
    df=data.frame(root_id=ids)
    if(!is.null(version) || !is.null(timestamp))
      aedes_main$root_id=fafbseg::flywire_updateids(aedes_main$root_id, svids = aedes_main$supervoxel_id, version = version, timestamp = timestamp)
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
    df$root_id=fafbseg::flywire_updateids(df$root_id, svids = df$supervoxel_id, version = version, timestamp = timestamp)
  }
  df
}

cam_meta_cols <- function(table, base=NULL, ...) {
  rr=flytable_query(glue::glue("select * from {table} limit 1"), base = base, ...)
  if(is.null(rr) || nrow(rr)<1) return(NULL)
  colnames(rr)
}
