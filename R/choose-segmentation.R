#' Choose or (temporarily) use a FAFB autosegmentation
#'
#' @details Each released segmentation implies a number of global options.
#' @param release character vector specifying a released segmentation.
#' @param set Whether or not to set the selected options for the selected
#'   \code{release}.
#'
#' @return If \code{set=TRUE} a list containing the previous values of the
#'   relevant global options (in the style of \code{\link{options}}. If
#'   \code{set=FALSE} a named list containing the option values.
#' @export
#'
#' @examples
#' choose_segmentation('20190805', set=FALSE)
choose_segmentation <- function(release=c('20190805', '20190521'), set=TRUE) {
  release <- match.arg(release)
  op <- if (release == '20190805') {
    list(
      fafbseg.sampleurl = "https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://neuroglancer-fafb-data/fafb_v14/fafb_v14_clahe_sharded%22%2C%22type%22:%22image%22%2C%22name%22:%22fafb_v14_clahe%22%7D%2C%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805%22%2C%22type%22:%22segmentation%22%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22name%22:%22fafb-ffn1-20190805%22%7D%2C%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805-skeletons32nm%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%220%22%2C%222252976277%22%5D%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22name%22:%22fafb-ffn1-20190805-skeletons32nm%22%2C%22visible%22:false%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B169903%2C49805%2C2763%5D%7D%7D%2C%22zoomFactor%22:8%7D%2C%22perspectiveOrientation%22:%5B-0.6771116256713867%2C0.6536111831665039%2C-0.1610027700662613%2C0.2973051071166992%5D%2C%22perspectiveZoom%22:2736.4687%2C%22showSlices%22:false%2C%22layout%22:%22xy-3d%22%7D",
      fafbseg.skeletonuri = "brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805-skeletons32nm",
      fafbseg.brainmaps.volume = "772153499790:fafb_v14:fafb-ffn1-20190805",
      fafbseg.brainmaps.meshName = "mcws_quad1e6",
      fafbseg.catmaid = "https://neuropil.janelia.org/tracing/fafb/v14-seg-li-190805.0/",
      fafbseg.skelziproot="fafb_ffn_20190805_flat_skeleton32nm512_nnconn215_mc10000_e250_prune10_thresh1000_sparse250"
    )
  } else if (release == '20190521') {
    list(
      fafbseg.sampleurl = "https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://neuroglancer-fafb-data/fafb_v14/fafb_v14_clahe_sharded%22%2C%22type%22:%22image%22%2C%22name%22:%22fafb_v14_clahe%22%7D%2C%7B%22source%22:%22precomputed://gs://fafb-ffn1-20190521/segmentation%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%220%22%2C%221366959786%22%5D%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22name%22:%22fafb-ffn1-20190521%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B132181.4375%2C44958.3515625%2C1586.5582275390625%5D%7D%7D%2C%22zoomFactor%22:8%7D%2C%22perspectiveOrientation%22:%5B-0.14806526899337769%2C0.06667085736989975%2C0.30171263217926025%2C0.9394685626029968%5D%2C%22perspectiveZoom%22:1693.2813601504033%2C%22showSlices%22:false%2C%22layout%22:%22xy-3d%22%7D",
      fafbseg.skeletonuri = "precomputed://gs://fafb-ffn1-20190521/segmentation/skeletons_32nm",
      fafbseg.brainmaps.volume = "772153499790:fafb_v14:fafb-ffn1-20190521",
      fafbseg.brainmaps.meshName = "mcws_quad1e6",
      # nb note that this URL is correct even though there is a date mismatch
      fafbseg.catmaid = "https://neuropil.janelia.org/tracing/fafb/v14seg-Li-190411.0/",
      fafbseg.skelziproot = "fafb_ffn_20190522_flat_skeleton32nm512_nnconn215_mc10000_e250_prune10_thresh1000_sparse250"
    )
  } else
    stop("Unknown segmentation!")


  if(!is.null(bd <-getOption("fafbseg.basedir"))){
    op$fafbseg.basedir=bd
  } else if(!is.null(zr <- getOption("fafbseg.skelziproot"))){
    op$fafbseg.basedir=dirname(zr)
  } else if(checkmate::test_directory_exists(bd <- path.expand("~/projects/fafbseg"))) {
    op$fafbseg.basedir=bd
  } else op$fafbseg.skelziproot=NULL

  if(isTRUE(nzchar(op$fafbseg.basedir))){
    op$fafbseg.skelziproot=file.path(op$fafbseg.basedir, op$fafbseg.skelziproot)
  } else op$fafbseg.skelziproot=NULL

  op$fafbseg.baseurl=sub("^([^#]+)/#!.*","\\1",op$fafbseg.sampleurl)
  if(isTRUE(set)) options(op) else op
}

#' @rdname choose_segmentation
#' @description \code{with_segmentation} allows a specific segmentation to be
#'   temporarily selected.
#' @param expr The expression to execute with the temporary options set
#'
#' @return The result of evaluating \code{expr}
#' @export
#'
#' @examples
#' \donttest{
#' n <- with_segmentation("20190521",{
#'   read.neuron.brainmaps(22427007374)
#' })
#' }
with_segmentation <- function(release, expr) {
  op <- choose_segmentation(release, set = TRUE)
  on.exit(options(op))
  force(expr)
}

find_zip_divisor <- memoise::memoise(function(zipdir=getOption("fafbseg.skelziproot")) {
  if (isFALSE(checkmate::test_directory_exists(zipdir)))
    stop(
      call. = FALSE,
      "Cannot find folder containing skeleton zip files!\n",
      "Please check value of fafbseg.skelziproot option, currently set as follows:",
      "\n\n  options(fafbseg.skelziproot=", deparse(zipdir), ")\n\n",
      "See ?fafbseg for details."
    )
  zips = dir(zipdir, pattern = '\\.zip$', full.names = T)
  if (length(zips)) {
    # if there are some zip files, then list one and figure out the
    # divisor that converts segment ids to zip files
    zip1 = zips[1]
    zl = zip_list(zip1)
    swc = zl[['filename']][1]
    signif(swc2segmentid(swc) / zip2segmentstem(zip1), digits = 1)
  } else NULL
})
