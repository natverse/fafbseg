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
#'   \item{\code{fafbseg.brainmaps_xyz2id.chunksize}} this will default to
#'   querying 4000 vertices at a time. Set this smaller if the queries time out
#'   or larger to speed things up. See
#'
#'   }
#'
#' @examples
#' options()[grep("^fafbseg\\.", names(options()))]
"_PACKAGE"
