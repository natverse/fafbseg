check_cave <- memoise::memoise(function(min_version=NULL) {
  check_reticulate()
  tryCatch(
    cv <- reticulate::import("caveclient"),
    error = function(e) {
      stop(
        call. = F,
        "Please install the python caveclient package:\n",
        "This should normally work:\n",
        "fafbseg::simple_python('full')\n",
        "For more details see ?simple_python"
      )
    }
  )
  if(!is.null(min_version)) {
    warning("min_version not yet implemented for caveclient")
  #   cvv=numeric_version(cloudvolume_version())
  #   if(!isTRUE(cvv >= min_version))
  #     stop("You need cloudvolume version: ", min_version, " but you have: ", cvv,
  #          "\n  Please update e.g. using\n",
  #          "fafbseg::simple_python('basic')")
  }
  cv
})



#' Low level access to the Flywire CAVE annotation system
#'
#' @details This depends on installation of the Python caveclient library. See
#'   \code{\link{flywire_cave_query}} for more details.
#' @param datastack_name defaults to "flywire_fafb_production". See
#'   \url{https://global.daf-apis.com/info/} for other options.
#' @return The \code{caveclient.frameworkclient.CAVEclientFull} Python module
#'   wrapped by reticulate.
#' @export
#'
#' @examples
#' \donttest{
#' fac <- flywire_cave_client()
#' fac$annotation$get_tables()
#' fac$annotation$get_table_metadata('nuclei_v1')
#'
#' # annotation tables that have been materialised in order to map XYZ
#' # points onto current root ids (and immutable supervoxel ids)
#' # typically the same as fac$annotation$get_tables()
#' fac$materialize$get_tables()
#'
#' info=fac$info$get_datastack_info()
#' # the default synapse table for the dataset
#' info$synapse_table
#' }
flywire_cave_client <- memoise::memoise(function(datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  cavec=check_cave()
  client = cavec$CAVEclient(datastack_name)
})

#' Query the FlyWire CAVE annotation system
#'
#' @details CAVE (Connectome Annotation Versioning Engine) provides a shared
#'   infrastructure for a number of connectomics projects involving Sebastian
#'   Seung's group at Princeton and collaborators at the Allen Institute. There
#'   is both a backend system running on their servers and a Python client for
#'   end users.
#'
#'   You can find out more at \url{https://caveclient.readthedocs.io/} as well
#'   as looking at the Python notebooks on the github repo
#'   \url{https://github.com/seung-lab/CAVEclient}.
#'
#'   The annotation system shares authentication infrastructure with the rest of
#'   the FlyWire API (see \code{\link{flywire_set_token}}).
#' @param table The name of the table to query
#' @param live Whether to use live query mode, which updates any root ids to
#'   their current value.
#' @param ... Additional arguments to the query method. See examples and
#'   details.
#' @inheritParams flywire_cave_client
#'
#' @return A \code{tibble}. Note that xyzmatrix can be used on single columns
#'   containing XYZ locations.
#' @export
#' @seealso \code{\link{flywire_cave_client}}
#' @examples
#' \donttest{
#' # note use of limit to restrict the number of rows
#' n10=flywire_cave_query(table = 'nuclei_v1', limit=10)
#' head(as.data.frame(n10))
#' }
#' \dontrun{
#' nuclei_v1=flywire_cave_query(table = 'nuclei_v1')
#' points3d(xyzmatrix(nuclei_v1$pt_position))
#'
#' library(elmr)
#' # calculate signed distance to FAFB surface
#' # NB this surface is not a perfect fit in the optic lobes
#' nuclei_v1$d=pointsinside(xyzmatrix(nuclei_v1$pt_position), FAFB.surf,
#'   rval = 'dist')
#' points3d(xyzmatrix(nuclei_v1$pt_position),
#'   col=matlab::jet.colors(20)[cut(nuclei_v1$d,20)])
#' plot3d(FAFB)
#' }
flywire_cave_query <- function(table,
                               datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
                               live=TRUE, ...) {
  check_package_available('arrow')
  fac=flywire_cave_client(datastack_name=datastack_name)
  # Live query updates ids
  # materialization_version=materialization_version
  annotdf <- if(live) {
    ts=format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6", tz="UTC")
    reticulate::py_call(fac$materialize$live_query, table=table, timestamp=ts, ...)
  } else {
    reticulate::py_call(fac$materialize$query_table, table=table, ...)
  }
  pandas2df(annotdf)
}

flywire_partners_cave <- function(rootid, partners=c("outputs", "inputs"),
                                  cleft.threshold=0,
                                  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
                                  synapse_table=NULL,
                                  fafbseg_colnames=TRUE, ...) {
  checkmate::assert_integerish(cleft.threshold,
                               lower = 0L, upper = 255L, len = 1)
  if(length(rootid)>1)
    rootid=paste(rootid, collapse = ',')
  partners=match.arg(partners)
  fac=flywire_cave_client(datastack_name=datastack_name)
  if(is.null(synapse_table)) {
    synapse_table=fac$info$get_datastack_info()[['synapse_table']]
    if(!isTRUE(nzchar(synapse_table)))
      stop("Unable to identify synapse table for datastack: ", datastack_name)
  }

  dict=sprintf('{"%s": [%s]}',
               ifelse(partners=="outputs", "pre_pt_root_id", "post_pt_root_id"),
               as.character(rootid))
  res=flywire_cave_query(datastack_name = datastack_name,
                         table = synapse_table,
                         filter_in_dict=reticulate::py_eval(dict, convert = F), ...)
  # FIXME - integrate into CAVE query
  if(cleft.threshold>0)
    res=res[res$cleft_score>cleft.threshold,,drop=FALSE]

  if(fafbseg_colnames) {
    colnames(res)[1]='offset'
    colnames(res)=sub("pt_supervoxel_id", "svid", colnames(res))
    colnames(res)=sub("pt_root_id", "id", colnames(res))
    colnames(res)[colnames(res)=='connection_score']='scores'
    colnames(res)[colnames(res)=='cleft_score']='cleft_scores'
  }
  res
}

# retained in case it is useful somewhere else ...
update_rootids <- function(rootids, svids) {
  stopifnot(bit64::is.integer64(rootids))
  stopifnot(bit64::is.integer64(svids))
  outofdate=!flywire_islatest(rootids)
  if(any(outofdate)) {
    rootids[outofdate]=flywire_rootid(svids[outofdate], integer64 = TRUE)
  }
  rootids
}


cave_latestid <- function(rootids, integer64=FALSE,
                           datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  fac=flywire_cave_client(datastack_name=datastack_name)
  rids=ngl_segments(rootids, as_character=T)
  res=reticulate::py_call(fac$chunkedgraph$get_latest_roots, rids2pyint(rids))
  newids=pyids2bit64(res, as_character = !integer64)
  newids
}
