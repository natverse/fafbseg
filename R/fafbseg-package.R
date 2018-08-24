#' @keywords internal
#' @section Package Options: \itemize{
#'
#'   \item{\code{fafbseg.skelziproot}} set to the location of a folder
#'   containing the zipped versions of the skeletonised segmentations. This will
#'   be used by \code{\link{read_segments}}, \code{\link{read_topn}} etc.
#'
#'   \item{\code{fafbseg.sampleurl}} optionally set to a sample Neuroglancer URL
#'   that will modified to point to arbitrary locations by
#'   \code{\link{open_fafb_ngl}}.
#'
#'   }
#'
#' @examples
#' options()[grep("^fafbseg\\.", names(options()))]
"_PACKAGE"
