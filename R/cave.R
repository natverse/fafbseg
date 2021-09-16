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
#'   \code{\link{flywire_cave_query for more details}}.
#'
#' @return
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
flywire_cave_client <- memoise::memoise(function(datastack_name = "flywire_fafb_production") {
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
#' @param datastack_name defaults to "flywire_fafb_production". See
#'   \url{https://global.daf-apis.com/info/} for other options.
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
#' @seealso
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
flywire_cave_query <- function(table, datastack_name = "flywire_fafb_production",
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
  tf=tempfile(fileext = '.feather')
  on.exit(unlink(tf))
  annotdf$to_feather(tf)
  annotdf.r=arrow::read_feather(tf)
  annotdf.r
}
