# Support for queries from canned flywire connectivity datasets

flywire_connectome_basedir <- function(d=getOption('fafbseg.flywire_connectome_dir', NULL)) {
  if(is.null(d))
    d="/Volumes/GoogleDrive/.shortcut-targets-by-id/1g7i3LMmDFcZXDXzevy3eUSrmcMJl2B6a/flywire_connectome_analysis_data"
  if(!file.exists(d))
    stop("Unable to find flywire connectome data at folder:", d,
         "\nPlease set options(fafbseg.flywire_connectome_dir='') correctly")
  d
}

flywire_connectome_latest <- memoise::memoise(function() {
  d=flywire_connectome_basedir()
  dd=dir(d, include.dirs = T, full.names = T)
  dd=dd[file.info(dd)$isdir]
  ddnum=suppressWarnings(as.integer(basename(dd)))
  seldir=dd[which.max(ddnum)]
  seldir
}, ~ memoise::timeout(3600))

flywire_connectome_dir <- function(version=NULL, cached=TRUE) {
  if(is.null(version)) {
    if(!cached)
      memoise::forget(flywire_connectome_latest)
    flywire_connectome_latest()
  } else {
    d=file.path(flywire_connectome_basedir(), version)
    if(!file.exists(d))
      stop("Unable to find flywire connectome data for that version!")
    d
  }
}

flywire_connectome_file <- function(type=c("syn", "pre", "post"), version=NULL, cached=TRUE) {
  type=match.arg(type)
  d=flywire_connectome_dir(version=version, cached=cached)
  version=basename(d)
  f=sprintf(
    switch(type,
         syn='syn_proof_analysis_filtered_%s.feather',
         pre='per_neuron_neuropil_filtered_count_pre_%s.feather',
         post='per_neuron_neuropil_filtered_count_post_%s.feather'),
    version)
  df=file.path(d, f)
  if(!file.exists(df))
    stop("Path: ", df, " does not exist!")
  df
}

#' Access precomputed flywire connectivity data
#'
#' @details This depends on precomputed data dumps prepared periodically by Sven
#'   Dorkenwald. You must either download these to an appropriate location on
#'   your hard drive or add a link to your own google drive. The link to Sven's
#'   google drive folder can be found in
#'   \href{https://flywire-forum.slack.com/archives/C01M4LP2Y2D/p1644529750249139}{this
#'    Slack message}.
#'
#' @param type Character vector specifying the kind of data
#' @param version Optional CAVE version. The default value of \code{NULL} uses
#'   the latest version available.
#' @param cached When version is \code{NULL} whether to use a cached value
#'   (lasting 1 hour) of the latest available version.
#'
#' @return A
#' @export
#'
#' @examples
#' \donttest{
#' syn450=try(flywire_connectome_data('syn', version=450), silent=T)
#' if(!inherits(syn450, 'try-error')) {
#' syn450
#' syn450$metadata
#'
#' dl4ds <- syn450 %>%
#'   filter(pre_pt_root_id==flywire_ids("DL4_adPN_R", version=450, integer64 = T)) %>%
#'   collect()
#'
#' }
#'
#' }
flywire_connectome_data <- function(type=c("syn", "pre", "post"), version=NULL, cached=TRUE, ...) {
  check_package_available('arrow')
  f=flywire_connectome_file(type, version = version, cached = cached)
  ds=arrow::open_dataset(f, format = 'arrow', ...)
  attr(ds, "version")=basename(dirname(f))
  ds
}

#' @importFrom dplyr collect rename arrange desc summarise
flywire_partner_summary2 <- function(ids, partners=c("outputs", "inputs"),
                                     add_cell_types=TRUE,
                                     summarise=FALSE, version=NULL,
                                     threshold=0) {
  partners=match.arg(partners)
  syn <- flywire_connectome_data("syn", version = version)
  version=attr(syn, 'version')
  ids <- flywire_ids(ids, version=version, integer64 = T)
  syn1 <- if(partners=='outputs') {
    syn %>%
    filter(pre_pt_root_id %in% ids)
  } else {
    syn %>%
    filter(post_pt_root_id %in% ids)
  }
  partner_col=ifelse(partners=='outputs', "post_pt_root_id", "pre_pt_root_id")
  syn2 <- syn1 %>%
    collect() %>%
    rename(weight=syn_count) %>%
    arrange(desc(weight))
  if(summarise) {
    syn2 <- syn2 %>%
    group_by(pre_pt_root_id, post_pt_root_id) %>%
    summarise(weight = sum(weight), top_np = neuropil[1])
  }
  res <- syn2 %>%
    filter(weight>threshold) %>%
    arrange(desc(weight))
  if(add_cell_types)
    res <- add_celltype_info(res, idcol=partner_col, version=version)
  attr(res, "version")=version
  res
}


flywire_adjacency_matrix2 <- function(rootids = NULL, inputids = NULL,
                                     outputids = NULL, sparse = TRUE,
                                     threshold=0,
                                     version=NULL,
                                     Verbose=interactive()) {
  syn <- flywire_connectome_data("syn", version = version)
  version=attr(syn, 'version')
  if (is.null(rootids)) {
    if (is.null(inputids) || is.null(outputids))
      stop("You must either specify bodyids OR (inputids AND outputids)!")
    inputids = flywire_ids(inputids, version=version, integer64 = T, unique = T)
    outputids = flywire_ids(outputids, version=version, integer64 = T, unique = T)
  } else {
    if (!is.null(inputids) || !is.null(outputids))
      stop("You must either specify bodyids OR (inputids AND outputids)!")
    inputids <- flywire_ids(rootids, version=version, integer64 = T, unique = T)
    outputids <- inputids
  }

  dd <- syn %>%
    filter(pre_pt_root_id %in% inputids) %>%
    filter(post_pt_root_id %in% outputids) %>%
    collect() %>%
    group_by(pre_pt_root_id, post_pt_root_id) %>%
    summarise(weight = sum(syn_count), top_np = neuropil[1])

  # if(remove_autapses) {
  #   # first case is when we have different input/output id sets
  #   dd <- if(is.null(rootids))
  #     filter(dd,inputids[.data$pre_rootidx]!=outputids[.data$post_rootidx])
  #   else
  #     filter(dd, .data$pre_rootidx!=.data$post_rootidx)
  # }

  sm = sparseMatrix(
    i = match(dd$pre_pt_root_id, inputids),
    j = match(dd$post_pt_root_id, outputids),
    dims = c(length(inputids), length(outputids)),
    x = dd$weight,
    dimnames = list(as.character(inputids), as.character(outputids))
  )
  if (isTRUE(sparse))
    sm
  else as.matrix(sm)
}

