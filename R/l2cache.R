#' Query FlyWire level 2 cache attributes
#'
#' @param rootid One or more FlyWire root ids defining a segment. Supply either
#'   \code{rootid} or \code{l2ids}.
#' @param l2ids Optional vector of level 2 ids to query directly. Supply either
#'   \code{rootid} or \code{l2ids}.
#' @param attributes Optional character vector of attribute names to retrieve.
#'   \code{NULL} requests all available attributes.
#' @param split_columns Whether to expand multivalued attributes into separate
#'   columns using the same names as python \code{caveclient}.
#' @param rval Return either a \code{data.frame} or the raw named \code{list}
#'   returned by python \code{caveclient}.
#' @param ... Additional arguments passed to \code{\link{flywire_l2ids}} and to
#'   recursive calls when \code{rootid} contains multiple ids.
#' @inheritParams flywire_cave_client
#'
#' @return When \code{rval="data.frame"}, a tibble with one row per queried
#'   level 2 id and an explicit \code{l2_id} column stored as a 64 bit integer.
#'   Other integer-valued columns are normalised for stability across
#'   \code{caveclient} code paths: values exactly representable as an R double
#'   are returned as numeric, while larger values are kept as
#'   \code{bit64::integer64}. When \code{rval="list"}, the raw named list
#'   returned by \code{caveclient$l2cache$get_l2data()}. When \code{rootid} has
#'   length greater than 1, a named list is returned with one result per root
#'   id.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- flywire_l2attributes(
#'   rootid = "720575940604351334",
#'   attributes = c("size_nm3", "rep_coord_nm")
#' )
#' head(x)
#' }
flywire_l2attributes <- function(
    rootid = NULL,
    l2ids = NULL,
    attributes = NULL,
    split_columns = TRUE,
    rval = c("data.frame", "list"),
    datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
    ...) {
  rval = match.arg(rval)
  if(!is.null(attributes) && !is.character(attributes))
    stop("attributes must be NULL or a character vector")
  if(is.null(rootid) == is.null(l2ids))
    stop("Please supply exactly one of rootid or l2ids")

  if(!is.null(rootid)) {
    rootid = flywire_ids(rootid, integer64 = FALSE)
    if(length(rootid) > 1L) {
      res = pbapply::pblapply(
        rootid,
        flywire_l2attributes,
        attributes = attributes,
        split_columns = split_columns,
        rval = rval,
        datastack_name = datastack_name,
        ...
      )
      names(res) = rootid
      return(res)
    }
    l2ids = flywire_l2ids(rootid, integer64 = TRUE, datastack_name = datastack_name, ...)
  }

  fcc = flywire_cave_client(datastack_name = datastack_name)
  pyids = rids2pyint(l2ids)
  pyattrs = if(is.null(attributes)) NULL else as.list(attributes)

  if(rval == "list") {
    res = reticulate::py_call(
      fcc$l2cache$get_l2data,
      l2_ids = pyids,
      attributes = pyattrs
    )
    return(reticulate::py_to_r(res))
  }

  if(reticulate::py_has_attr(fcc$l2cache, "get_l2data_table")) {
    res = reticulate::py_call(
      fcc$l2cache$get_l2data_table,
      l2_ids = pyids,
      attributes = pyattrs,
      split_columns = split_columns
    )
    return(l2cache_normalise_table(pandas2df(res, keep_index = TRUE, tibble = TRUE)))
  }

  res = reticulate::py_call(
    fcc$l2cache$get_l2data,
    l2_ids = pyids,
    attributes = pyattrs
  )
  l2cache_normalise_table(
    l2cache_data_to_tibble(reticulate::py_to_r(res), split_columns = split_columns)
  )
}

#' Return FlyWire level 2 cache metadata
#'
#' @inheritParams flywire_cave_client
#'
#' @return A named list mapping available level 2 cache attributes to their
#'   declared data types.
#' @export
flywire_l2cache_metadata <- function(
    datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  fcc = flywire_cave_client(datastack_name = datastack_name)
  fcc$l2cache$cache_metadata()
}

#' Check whether an L2 cache is available
#'
#' @inheritParams flywire_cave_client
#'
#' @return Logical scalar indicating whether the current datastack has an
#'   associated level 2 cache.
#' @export
flywire_has_l2cache <- function(
    datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  fcc = flywire_cave_client(datastack_name = datastack_name)
  fcc$l2cache$has_cache(datastack_name = datastack_name)
}

#' Sum level 2 volumes for one or more FlyWire root ids
#'
#' @param rootids One or more FlyWire root ids.
#' @param cache Whether to cache rootid-level volume totals on disk.
#' @param ... Additional arguments passed to \code{\link{flywire_l2attributes}}
#'   and to recursive calls when \code{rootids} has length greater than one.
#' @inheritParams flywire_cave_client
#'
#' @return A numeric scalar volume in nm^3 for one input root id, or a named
#'   vector when \code{rootids} has length greater than one.
#' @export
flywire_l2volume <- function(
    rootids,
    cache = TRUE,
    datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
    ...) {
  rootids = flywire_ids(rootids, integer64 = FALSE)
  fcc = flywire_cave_client(datastack_name = datastack_name)
  if(length(rootids) > 1L) {
    return(pbapply::pbsapply(
      rootids,
      flywire_l2volume,
      cache = cache,
      datastack_name = datastack_name,
      ...
    ))
  }

  vol_cache = flywire_leaves_cache(
    subdir = file.path("flywire_l2volume", fcc$datastack_name)
  )
  vol = if(cache) vol_cache$get(rootids) else cachem::key_missing()
  if(cachem::is.key_missing(vol)) {
    vols = flywire_l2attributes(
      rootid = rootids,
      attributes = "size_nm3",
      datastack_name = datastack_name,
      ...
    )
    vol = sum(as.numeric(vols$size_nm3), na.rm = TRUE)
    if(cache)
      vol_cache$set(rootids, vol)
  }
  vol
}

l2cache_data_to_tibble <- function(x, split_columns = TRUE) {
  if(!length(x))
    return(tibble::tibble(l2_id = bit64::as.integer64(character())))

  df = list2df(unname(x), lists = "list")
  df$l2_id = bit64::as.integer64(names(x))
  df = tibble::as_tibble(df[c("l2_id", setdiff(names(df), "l2_id"))])

  if(!split_columns)
    return(df)

  df = l2cache_expand_column(
    df,
    "rep_coord_nm",
    c("rep_coord_nm_x", "rep_coord_nm_y", "rep_coord_nm_z")
  )
  df = l2cache_expand_column(
    df,
    "pca",
    c(
      "pca_0_x", "pca_0_y", "pca_0_z",
      "pca_1_x", "pca_1_y", "pca_1_z",
      "pca_2_x", "pca_2_y", "pca_2_z"
    )
  )
  df = l2cache_expand_column(
    df,
    "pca_val",
    c("pca_val_0", "pca_val_1", "pca_val_2")
  )
  l2cache_expand_column(
    df,
    "chunk_intersect_count",
    c(
      "chunk_intersect_count_x_bottom",
      "chunk_intersect_count_y_bottom",
      "chunk_intersect_count_z_bottom",
      "chunk_intersect_count_x_top",
      "chunk_intersect_count_y_top",
      "chunk_intersect_count_z_top"
    )
  )
}

l2cache_normalise_table <- function(df) {
  if(!nrow(df) || !ncol(df))
    return(df)

  if("l2_id" %in% names(df) && !bit64::is.integer64(df$l2_id))
    df$l2_id = bit64::as.integer64(as.character(df$l2_id))

  other_cols = setdiff(names(df), "l2_id")
  for(col in other_cols) {
    x = df[[col]]
    if(!bit64::is.integer64(x))
      next
    vals = as.character(x)
    nums = suppressWarnings(as.numeric(vals))
    if(all(is.na(nums) | abs(nums) < 2^53))
      df[[col]] = nums
  }

  df
}

l2cache_expand_column <- function(df, column, new_columns) {
  if(!column %in% names(df))
    return(df)

  vals = df[[column]]
  expanded = t(vapply(
    vals,
    l2cache_flatten_value,
    FUN.VALUE = numeric(length(new_columns)),
    n = length(new_columns)
  ))
  expanded = as.data.frame(expanded, stringsAsFactors = FALSE)
  names(expanded) = new_columns
  expanded = tibble::as_tibble(expanded)
  df[[column]] = NULL
  tibble::as_tibble(cbind(df, expanded))
}

l2cache_flatten_value <- function(x, n) {
  if(is.null(x) || (length(x) == 1L && isTRUE(is.na(x))))
    return(rep(NA_real_, n))

  if(is.matrix(x) || length(dim(x)) == 2L)
    x = as.numeric(t(x))
  else
    x = as.numeric(x)

  length(x) = n
  x
}
