#' Choose or (temporarily) use a FAFB autosegmentation
#'
#' @details Each released segmentation implies a number of global options. This
#'   package comes with 4 different default scene urls specified via
#'   \code{\link{choose_segmentation}} or \code{\link{with_segmentation}}. This
#'   is the easiest way to choose a particular segmentation. You can also pass a
#'   sample URL to the \code{release} argument. As of Nov 2020, the
#'   \code{"flywire31"} is the default segmentation when the package loads.
#'
#'   You can also specify a different sample URL via the \code{sampleurl}
#'   argument of some functions; it will be remembered for the rest of the R
#'   session. If you regularly use a particular kind of scene URL, you can set
#'   \code{options(fafbseg.sampleurl)} in your \code{\link{Rprofile}} file.
#'
#'   If you need to use both built-in and custom segmentation URLs, we recommend
#'   specifying the custom URL in your \code{\link{Rprofile}} file and using the
#'   \code{with_segmentation} to run code that uses one of the built-in
#'   segmentations.
#'
#' @param release character vector specifying a released segmentation via a
#'   known short name or a sample neuroglancer URL.
#' @param set Whether or not to set the selected options for the selected
#'   \code{release}.
#' @param moreoptions Any further options that you might wish to set (optional,
#'   expert use only, principally intended for use by \code{fancr} package).
#'
#' @return If \code{set=TRUE} a list containing the previous values of the
#'   relevant global options (in the style of \code{\link{options}}. If
#'   \code{set=FALSE} a named list containing the option values.
#' @export
#'
#' @examples
#' \donttest{
#' choose_segmentation('20190805', set=FALSE)
#' }
#' \dontrun{
#' # temporarily change default segmentation to run a command
#' # but restore original default when finished
#' # Choose the FlyWire segmentation
#' with_segmentation('flywire31', {open_fafb_ngl(c(460792, 221812, 61480))})
#'
#' # similarly for one Google (Li and Jain) segmentation
#' with_segmentation('20190805', {open_fafb_ngl(c(460792, 221812, 61480))})
#' }
choose_segmentation <- function(release=c('flywire31', '20200412', '20190805',
                                          '20190521', 'sandbox-flywire31'),
                                set=TRUE, moreoptions=list()) {
  if(length(release)==1 && isTRUE(grepl("^http", release))) {
    op <- list(fafbseg.sampleurl=release)
  } else {
  release <- match.arg(release)
  op <- if (release == '20190805') {
    list(
      fafbseg.sampleurl = "https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://neuroglancer-fafb-data/fafb_v14/fafb_v14_clahe_sharded%22%2C%22type%22:%22image%22%2C%22name%22:%22fafb_v14_clahe%22%7D%2C%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805%22%2C%22type%22:%22segmentation%22%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22name%22:%22fafb-ffn1-20190805%22%7D%2C%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805-skeletons32nm%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%220%22%5D%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22name%22:%22fafb-ffn1-20190805-skeletons32nm%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B122437.5625%2C36391.234375%2C2057.085205078125%5D%7D%7D%2C%22zoomFactor%22:8%7D%2C%22perspectiveOrientation%22:%5B-0.6771116256713867%2C0.6536111831665039%2C-0.1610027700662613%2C0.2973051071166992%5D%2C%22perspectiveZoom%22:5184.860830857428%2C%22showSlices%22:false%2C%22layout%22:%224panel%22%7D",
      fafbseg.skeletonuri = "brainmaps://772153499790:fafb_v14:fafb-ffn1-20190805-skeletons32nm",
      fafbseg.brainmaps.volume = "772153499790:fafb_v14:fafb-ffn1-20190805",
      fafbseg.brainmaps.meshName = "mcws_quad1e6",
      fafbseg.catmaid = "https://garden.catmaid.org/tracing/fafb/v14-seg-li-190805.0/",
      fafbseg.skelziproot="fafb_ffn_20190805_flat_skeleton32nm512_nnconn215_mc10000_e250_prune10_thresh1000_sparse250"
    )
  } else if (release == '20190521') {
    list(
      fafbseg.sampleurl = "https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://neuroglancer-fafb-data/fafb_v14/fafb_v14_clahe_sharded%22%2C%22type%22:%22image%22%2C%22name%22:%22fafb_v14_clahe%22%7D%2C%7B%22source%22:%22precomputed://gs://fafb-ffn1-20190521/segmentation%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%220%22%2C%221366959786%22%5D%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22name%22:%22fafb-ffn1-20190521%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B132181.4375%2C44958.3515625%2C1586.5582275390625%5D%7D%7D%2C%22zoomFactor%22:8%7D%2C%22perspectiveOrientation%22:%5B-0.14806526899337769%2C0.06667085736989975%2C0.30171263217926025%2C0.9394685626029968%5D%2C%22perspectiveZoom%22:1693.2813601504033%2C%22showSlices%22:false%2C%22layout%22:%22xy-3d%22%7D",
      fafbseg.skeletonuri = "precomputed://gs://fafb-ffn1-20190521/segmentation/skeletons_32nm",
      fafbseg.brainmaps.volume = "772153499790:fafb_v14:fafb-ffn1-20190521",
      fafbseg.brainmaps.meshName = "mcws_quad1e6",
      # nb note that this URL is correct even though there is a date mismatch
      fafbseg.catmaid = "https://garden.catmaid.org/tracing/fafb/v14seg-Li-190411.0/",
      fafbseg.skelziproot = "fafb_ffn_20190522_flat_skeleton32nm512_nnconn215_mc10000_e250_prune10_thresh1000_sparse250"
    )
  } else if (release=='flywire31') {
    list(fafbseg.sampleurl = "https://ngl.flywire.ai/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked%22%2C%22type%22:%22image%22%2C%22blend%22:%22default%22%2C%22shaderControls%22:%7B%7D%2C%22name%22:%22Production-image%22%7D%2C%7B%22source%22:%22graphene://https://prod.flywire-daf.com/segmentation/table/fly_v31%22%2C%22type%22:%22segmentation_with_graph%22%2C%22segments%22:%5B%220%22%5D%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22graphOperationMarker%22:%5B%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%2C%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%5D%2C%22pathFinder%22:%7B%22color%22:%22#ffff00%22%2C%22pathObject%22:%7B%22annotationPath%22:%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%2C%22hasPath%22:false%7D%7D%2C%22name%22:%22Production-segmentation_with_graph%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B108360%2C42086%2C3279%5D%7D%7D%2C%22zoomFactor%22:8%7D%2C%22perspectiveZoom%22:2230.6094286986126%2C%22jsonStateServer%22:%22https://globalv1.flywire-daf.com/nglstate/post%22%2C%22selectedLayer%22:%7B%22layer%22:%22Production-segmentation_with_graph%22%2C%22visible%22:true%7D%2C%22layout%22:%22xy-3d%22%7D",
         fafbseg.cave.datastack_name="flywire_fafb_production")
  } else if(release=='sandbox-flywire31') {
    list(fafbseg.sampleurl = "https://ngl.flywire.ai/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked%22%2C%22type%22:%22image%22%2C%22blend%22:%22default%22%2C%22shaderControls%22:%7B%7D%2C%22name%22:%22Sandbox-image%22%7D%2C%7B%22source%22:%22graphene://https://prodv1.flywire-daf.com/segmentation/table/fly_v26%22%2C%22type%22:%22segmentation_with_graph%22%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22graphOperationMarker%22:%5B%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%2C%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%5D%2C%22pathFinder%22:%7B%22color%22:%22#ffff00%22%2C%22pathObject%22:%7B%22annotationPath%22:%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%2C%22hasPath%22:false%7D%7D%2C%22name%22:%22sandbox-segmentation-FOR%20PRACTICE%20ONLY%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B108360%2C42086%2C3279%5D%7D%7D%2C%22zoomFactor%22:4%7D%2C%22perspectiveOrientation%22:%5B-0.0021646153181791306%2C0.000400538498070091%2C8.670138527122617e-7%2C0.9999975562095642%5D%2C%22perspectiveZoom%22:2585.0186809766333%2C%22jsonStateServer%22:%22https://globalv1.flywire-daf.com/nglstate/post%22%2C%22selectedLayer%22:%7B%22layer%22:%22sandbox-segmentation-FOR%20PRACTICE%20ONLY%22%7D%2C%22layout%22:%22xy-3d%22%7D",
         fafbseg.cave.datastack_name="flywire_fafb_sandbox"
            )
  } else if (release == '20200412') {
    list(
      fafbseg.sampleurl = "https://fafb-dot-neuroglancer-demo.appspot.com/#!%7B%22dimensions%22:%7B%22x%22:%5B4e-9%2C%22m%22%5D%2C%22y%22:%5B4e-9%2C%22m%22%5D%2C%22z%22:%5B4e-8%2C%22m%22%5D%7D%2C%22position%22:%5B109357.625%2C41309.41015625%2C5417%5D%2C%22crossSectionScale%22:2.1875%2C%22projectionOrientation%22:%5B0.8537589907646179%2C0.13250325620174408%2C-0.1204778179526329%2C-0.4889003336429596%5D%2C%22projectionScale%22:13886.509678558012%2C%22layers%22:%5B%7B%22type%22:%22image%22%2C%22source%22:%22precomputed://gs://neuroglancer-fafb-data/fafb_v14/fafb_v14_clahe%22%2C%22tab%22:%22source%22%2C%22name%22:%22fafb_v14_clahe%22%7D%2C%7B%22type%22:%22segmentation%22%2C%22source%22:%7B%22url%22:%22precomputed://gs://fafb-ffn1-20200412/segmentation%22%2C%22subsources%22:%7B%22default%22:true%2C%22bounds%22:true%2C%22mesh%22:true%2C%22skeletons%22:true%7D%2C%22enableDefaultSubsources%22:false%7D%2C%22tab%22:%22source%22%2C%22meshSilhouetteRendering%22:3%2C%22segments%22:%5B%22710435991%22%5D%2C%22name%22:%22fafb-ffn1-20200412%22%7D%5D%2C%22showAxisLines%22:false%2C%22showSlices%22:false%2C%22selectedLayer%22:%7B%22layer%22:%22fafb-ffn1-20200412%22%7D%2C%22layout%22:%22xy-3d%22%7D",
      fafbseg.catmaid = "https://spine.itanna.io/catmaid/fafb-v14-seg-li-200412.0/",
      fafbseg.brainmaps.volume = "772153499790:fafb_v14:fafb-ffn1-20200412-rc4",
      fafbseg.skeletonuri = "brainmaps://772153499790:fafb_v14:fafb-ffn1-20200412-rc4/skeleton_scale4")
    } else stop("Unknown segmentation!")
  }

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

  # this is the source for fetching meshes with cloudvolume
  op$fafbseg.cloudvolume.url=ngl_segmentation(op$fafbseg.sampleurl, rval='url')
  # set any extra options
  op[names(moreoptions)]=moreoptions
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
#' \dontrun{
#' n <- with_segmentation("20190521",
#'   read.neuron.brainmaps(22427007374))
#'
#' # open location in flywire
#' with_segmentation("flywire", open_fafb_ngl(c(433440, 168344, 131200)))
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
