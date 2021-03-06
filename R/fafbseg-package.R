#' @keywords internal
#' @section Package Options: \itemize{
#'
#'   \item{\code{fafbseg.skelziproot}} set to the location of a folder
#'   containing the zipped versions of the skeletonised segmentations. This will
#'   be used by \code{\link{read_segments}}, \code{\link{read_topn}} etc.
#'
#'   \item{\code{fafbseg.skeletonuri}} a brainmaps URI specifying a remote
#'   source used by \code{\link{read.neurons.brainmaps}} and
#'   \code{\link{brainmaps_skeleton}} to read neuronal skeletons.
#'
#'   \item{\code{fafbseg.sampleurl}} optionally set to a sample Neuroglancer URL
#'   that will modified to point to arbitrary locations by
#'   \code{\link{open_fafb_ngl}}.
#'
#'   \item{\code{fafbseg.sqlitepath}} optional to set the location of sqlite
#'   tables used by \code{\link{flywire_partners}} and friends.
#'
#'   \item{\code{fafbseg.cachedir}} The location for disk caches for functions
#'   including \code{\link{flywire_leaves}} If unset on package load, will be
#'   set to an appropriate user folder using
#'   \code{rappdirs::\link[rappdirs]{user_data_dir}}.
#'
#'   \item{\code{fafbseg.flcachesize}} The maximum cache size in bytes. Defaults
#'   to \code{1.5 * 1024^3} when unset. See \code{\link{flywire_leaves}} for
#'   details.
#'
#'   \item{\code{fafbseg.brainmaps_xyz2id.chunksize}} this will default to
#'   querying 4000 vertices at a time. Set this smaller if the queries time out
#'   or larger to speed things up. See \code{\link{brainmaps_xyz2id}} for
#'   details.
#'
#'   }
#'
#' @examples
#' options()[grep("^fafbseg\\.", names(options()))]
"_PACKAGE"
