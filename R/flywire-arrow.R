# Support for queries from canned flywire connectivity datasets

flywire_connectome_basedir <- function(d=getOption('fafbseg.flywire_connectome_dir', NULL),
                                       create=NA, check_contents=!create) {

  if(is.null(d)) {
    if(is.na(create)) create=TRUE
    d=fafbseg_userdir('flywire_connectome_analysis_data')
  } else if(is.na(create)) create=FALSE

  if(!file.exists(d)) {
    if(create)
      dir.create(d, recursive = TRUE)
    else stop("Please set options(fafbseg.flywire_connectome_dir='') to point to the correct location of cached flywire connectome data.")
  }
  if(!isTRUE(check_contents)) return(d)
  subd=dir(d, include.dirs = T)
  if(!(length(subd)>0)) {
    # if(interactive() && grepl("darwin", R.version$os))
    #   system(paste("open", shQuote(d)))
    stop("No connection data found. Please run\ndownload_flywire_release_data()")
  }
  d
}

flywire_connectome_latest_nomemo <- function() {
  d=flywire_connectome_basedir()
  dd=dir(d, include.dirs = T, full.names = T)
  if(length(dd)==0) return(NA_character_)
  dd=dd[file.info(dd)$isdir]
  ddnum=suppressWarnings(as.integer(basename(dd)))
  seldir=dd[which.max(ddnum)]
  seldir
  version=basename(seldir)
  if(as.numeric(version)<630) {
    warning("We recommend updating to connection data version 630. ",
            "You can do this by running\ndownload_flywire_release_data()")
  }
  seldir
}

flywire_connectome_latest <- memoise::memoise(flywire_connectome_latest_nomemo, ~ memoise::timeout(3600))

flywire_connectome_dir <- function(version=NULL, cached=TRUE, mustWork=TRUE) {
  if(is.null(version)) {
    if(!cached)
      memoise::forget(flywire_connectome_latest)
    flywire_connectome_latest()
  } else {
    d=file.path(flywire_connectome_basedir(), version)
    if(isTRUE(mustWork) && !file.exists(d))
      if(version %in% c(630, 783))
        stop("No connection data found for version 630 or 783. Please run\ndownload_flywire_release_data()") else
      stop("Unable to find flywire connectome data for that version!")
    d
  }
}

flywire_connectome_file <- function(type=c("syn", "pre", "post"), version=NULL,
                                    cached=TRUE, mustWork=TRUE) {
  type=match.arg(type)
  d=flywire_connectome_dir(version=version, cached=cached, mustWork=TRUE)
  version=basename(d)
  f=sprintf(
    switch(type,
         syn='syn_proof_analysis_filtered_%s.feather',
         pre='per_neuron_neuropil_filtered_count_pre_%s.feather',
         post='per_neuron_neuropil_filtered_count_post_%s.feather'),
    version)
  df=file.path(d, f)
  if(isTRUE(mustWork) && !file.exists(df))
    stop("Path: ", df, " does not exist!")
  df
}

#' Access precomputed flywire connectivity data
#'
#' @description \code{flywire_connectome_data} returns a \code{\link{dplyr}}
#'   compatible connection to connectivity dumps on disk.
#'
#' @details This depends on precomputed data dumps prepared periodically by Sven
#'   Dorkenwald. You can download the public release version using the function
#'   \code{\link{download_flywire_release_data}}.
#'
#'   You can download other versions from Sven's Google drive folder. See
#'   \href{https://flywire-forum.slack.com/archives/C01M4LP2Y2D/p1644529750249139}{this FlyWire
#'    Slack message} for more details and the URL.
#'
#' @param type Character vector specifying the kind of data
#' @param version Optional CAVE version. The default value of \code{NULL} uses
#'   the latest data dump available unless
#'   \code{options(fafbseg.flywire_connectome_data_version)} has been set (which
#'   you can conveniently do using
#'   \code{\link{flywire_connectome_data_version}()}).
#' @param cached When version is \code{NULL} whether to use a cached value
#'   (lasting 1 hour) of the latest available version.
#' @param ... Additional arguments passed to \code{arrow::open_dataset}.
#'
#' @return An arrow object that you can use with \code{dplyr} verbs like
#'   \code{filter} in order to find neurons/connectivity data of interest.
#' @export
#' @seealso \code{\link{download_flywire_release_data}}
#' @examples
#' \donttest{
#' # latest available version/
#' syn=try(flywire_connectome_data('syn'), silent=TRUE)
#' syn450=try(flywire_connectome_data('syn', version=450), silent=TRUE)
#' if(!inherits(syn450, 'try-error')) {
#' syn450
#' syn450$metadata
#'
#' dl4ds <- syn450 %>%
#'   filter(pre_pt_root_id==flywire_ids("DL4_adPN_L", version=450, integer64 = TRUE)) %>%
#'   collect()
#' }
#'
#' }
flywire_connectome_data <- function(type=c("syn", "pre", "post"),
                                    version=NULL,
                                    cached=TRUE, ...) {
  check_package_available('arrow')
  if(is.null(version))
    version=getOption("fafbseg.flywire_connectome_data_version")
  f=flywire_connectome_file(type, version = version, cached = cached)
  ds=arrow::open_dataset(f, format = 'arrow', ...)
  attr(ds, "version")=basename(dirname(f))
  ds
}


#' @export
#' @description \code{flywire_connectome_data_version} sets the integer version
#'   number of the preferred flywire connectome data dump or returns the
#'   currently version.
#'
#' @details Two pieces of information are used to determine the \emph{version} when it
#'   is queried. First the value of
#'   \code{options(fafbseg.flywire_connectome_data_version)}, second the latest
#'   available version of the connectivity dumps provided by
#'   \code{flywire_connectome_data()}.
#'
#' @param set When \code{set=<number>} is passed as an argument the specified
#'   data version will be used going forwards in this session as the default.
#'   This is achieved by setting the
#'   \code{fafbseg.flywire_connectome_data_version} option. When \code{set=NA}
#'   is specified then the option is cleared. When \code{set=FALSE}, the latest
#'   version on disk will be returned regardless of the value of
#'   \code{options("fafbseg.flywire_connectome_data_version"))}. See examples.
#' @param default A version to return when no other information is available.
#'   Defaults to \code{NA} to indicate no version information available.
#' @return An integer version number \emph{or} a list with the previous value of
#'   \code{options(fafbseg.flywire_connectome_data_version)} when
#'   \code{set=<number>}.
#' @rdname flywire_connectome_data
#' @examples
#' \dontrun{
#' # report active connectome dump version (defaults to most recent available)
#' flywire_connectome_data_version()
#'
#' # use the June 2023 public release version as the default
#' flywire_connectome_data_version(set=630)
#' # confirm this is the default
#' flywire_connectome_data_version()
#'
#' # check the latest version on disk
#' flywire_connectome_data_version(set=FALSE)
#'
#' # stop defaulting to specific version (therefore using the latest on disk)
#' flywire_connectome_data_version(set=NA)
#' flywire_connectome_data_version()
#' }
flywire_connectome_data_version <- function(set=NULL, default=NA) {
  if(!missing(set) && !isFALSE(set)) {
    if(is.null(set) || is.na(set))
      ver=NULL
    else ver=as.integer(checkmate::assert_integerish(set))
    op=options(fafbseg.flywire_connectome_data_version=ver)
    return(invisible(op))
  }
  ondisk <- as.integer(basename(flywire_connectome_dir()))
  op <- getOption('fafbseg.flywire_connectome_data_version')

  res <- if(isFALSE(set) || is.null(op)) ondisk else op
  if(!is.finite(res)) res <- default
  res
}

flywire_connectome_data_message <- function() {
  d=flywire_connectome_basedir()
  if(interactive() && grepl("darwin", R.version$os))
    system(paste("open", shQuote(d)))
  message("You can get flywire connectome data",

          "\nby downloading a numbered data folder (eg 630) from the Google drive link in this slack message",
          "\nhttps://flywire-forum.slack.com/archives/C01M4LP2Y2D/p1644529750249139",
          "\nand place it in in this folder:\n", d)
}


#' Rapid flywire connectivity summaries using cached connectome data
#'
#' @details Note that the threshold is applied to each row left after any
#'   grouping operations. Therefore when \code{by.roi=TRUE} only neuropil
#'   regions exceeding this threshold will be returned.
#'
#'   CAVE specifies versions (effectively timestamps) for the connectome data.
#'   Every so often Sven makes a dump of the connectivity and synapse
#'   information for all proofread neurons. In order to use the data
#'
#' @param ids Root ids to query (passed to \code{\link{flywire_ids}})
#' @param add_cell_types Whether to add cell type information to the result
#' @param by.roi Whether to break the connectivity down into rows for each
#'   neuropil region containing synapses.
#' @param summarise Whether to collapse down the results for multiple query
#'   neurons into a single entry for each partner neuron.
#' @inheritParams flywire_connectome_data
#' @inheritParams flywire_partner_summary
#' @importFrom dplyr ungroup n_distinct collect .data
#' @return A data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' flywire_partner_summary2('DA2_lPN', partners='out')
#' flywire_partner_summary2('DA2_lPN', partners='out', summarise=T)
#' flywire_partner_summary2('DA2_lPN', partners='out', summarise=T, by.roi=T)
#'
#' flywire_partner_summary2('DA2_lPN', partners='out', summarise=T,
#'   by.roi=T, add_cell_types=F) %>%
#'   filter(!grepl("AL", neuropil)) %>%
#'   group_by(post_pt_root_id) %>%
#'   summarise(weight = sum(weight), top_np = neuropil[1]) %>%
#'   arrange(desc(weight)) %>%
#'   # nb version = TRUE will use ensure that ids match the default CAVE version
#'   add_celltype_info(version=TRUE)
#' }
flywire_partner_summary2 <- function(ids, partners=c("outputs", "inputs"),
                                     add_cell_types=TRUE,
                                     by.roi=FALSE,
                                     summarise=FALSE,
                                     threshold=0,
                                     version=NULL) {
  partners=match.arg(partners)
  syn <- flywire_connectome_data("syn", version = version)
  version=attr(syn, 'version')
  ids <- flywire_ids(ids, version=version, integer64 = T)

  idcols=c("pre_pt_root_id", "post_pt_root_id")
  partner_col=if(partners=='inputs') idcols[1] else idcols[2]
  query_col=setdiff(idcols, partner_col)
  syn2 <- syn %>%
    filter(.data[[query_col]] %in% ids) %>%
    collect() %>%
    rename(weight=syn_count) %>%
    arrange(desc(weight))

  # In the wild we now have some summary data that does not have a neuropil col
  if(!"neuropil" %in% colnames(syn2)) {
    syn2$neuropil=NA_character_
  }

  syn2 <- if(by.roi && summarise) {
    # collapse query but leave neuropil info intact
    syn2 %>% group_by(across(all_of(c(partner_col, "neuropil")))) %>%
      summarise(weight = sum(weight), n=n_distinct(.data[[query_col]]))
  } else if(!by.roi && summarise) {
    # collapse query and neuropil info
    syn2 %>% group_by(across(all_of(partner_col))) %>%
      summarise(weight = sum(weight), n=n_distinct(.data[[query_col]]),
                top_np = neuropil[1])
  } else if(!by.roi && !summarise) {
    # leave separate query neurons intact but collapse neuropil info
    syn2 %>% group_by(pre_pt_root_id, post_pt_root_id) %>%
      summarise(weight = sum(weight), top_np = neuropil[1]) %>%
      ungroup()
  } else syn2

  res <- syn2 %>%
    filter(weight>threshold) %>%
    arrange(desc(weight))
  if(add_cell_types)
    res <- add_celltype_info(res, idcol=partner_col, version=version)
  attr(res, "version")=version
  res
}


#' Fast adjacency matrices based on flywire connectome dumps
#'
#' @inheritParams flywire_adjacency_matrix
#' @inheritParams flywire_partner_summary2
#'
#' @return A sparse matrix (\code{Matrix::dgCMatrix}) or regular \code{matrix}.
#' @export
#'
#' @examples
#' \dontrun{
#' dm2pnkc=flywire_adjacency_matrix2(inputids="DM2_lPN_R", outputids="class:Kenyon_Cell_R")
#' }
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

