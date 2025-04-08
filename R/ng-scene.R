#' Add a transformation to one or more layers in a neuroglancer scene
#'
#' @param x A neuroglancer scene as produced by \code{\link{ngl_decode_scene}}
#' @param reg A registration either as a \code{\link{reglist}} containing
#'   multiple registrations or a single registration in any form handled by
#'   \code{\link{xform}}.
#' @param layers A character vector specifying the layers in the scene to
#'   transform. If the elements are named, they names specify the \emph{new}
#'   names of the transformed layer.
#' @param ... Additional arguments passed to \code{\link{fit_xform}} when
#'   \code{reg} specifies a non-rigid registration. See \bold{details}.
#'
#' @details Neuroglancer only implements homogeneous affine transforms for
#'   layers. However these can still be quite useful when a non-rigid transform
#'   cannot be applied to a layer e.g. because the underlying neurons are
#'   undergoing rapid editing and it is not practical to generate a static set
#'   of transformed meshes.
#'
#' @return A new \code{ngscene} object
#' @importFrom nat.templatebrains fit_xform
#' @export
#'
#' @examples
#' \donttest{
#' # flywire scene
#' u='https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/4559706743898112'
#' scf=ngl_decode_scene(u)
#' m=matrix(c(-0.9663, -0.0695, 0.17, 0, 0.0351, 1.043, -0.028, 0, 0.1082,
#'   -0.0093, 0.9924, 0, 1021757.1284, 31409.0911, -85626.0572, 1), ncol=4)
#' # nb replaces existing layer of this name
#' xform(scf, m, layers=c('Production-mirrored'))
#'
#' scu=ngl_decode_scene('https://tinyurl.com/kj9rwn26')
#' # make a new layer mirroring an existing layer
#' scu2=xform(scu, m, layers=c('fly_v31_m'='fly_v31'))
#' scu2
#' }
#' \dontrun{
#' browseURL(as.character(scu2))
#' }
#'
#' \dontrun{
#' # mirror a flywire scene based on points from a specific pair of neurons
#' mbon18.dps=read_l2dp('MBON18')
#' mirror_reg=fit_xform(samplepts = mbon18.dps,
#'   refpts = nat.jrcbrains::mirror_fafb(mbon18.dps), subsample = 500)
#' flywire_scene('MBON18') %>%
#'   ngl_decode_scene %>%
#'   xform(mirror_reg, layers=c("mirror"="Production-segmentation_with_graph")) %>%
#'   as.character() %>%
#'   browseURL()
#' }
#' @importFrom nat xform
xform.ngscene <- function(x, reg, layers=NULL, ...) {
  if(inherits(reg, 'reglist')) {
    reg=nat::simplify_reglist(reg)
    if(length(reg)>1 || !is.matrix(reg[[1]]) || !identical(dim(reg[[1]]), c(4L,4L)))
      # need to compute a transform based on pts
      reg=fit_xform(reg, ...)
    else
      reg=reg[[1]]
  } else {
    stopifnot(is.matrix(reg))
  }
  stopifnot(identical(dim(reg), c(4L,4L)))
  # voxdim=x$navigation$pose$position$voxelSize
  scdims=x$dimensions
  if(!is.null(scdims)){
    voxdim=as.numeric(sapply(scdims, "[", 1))
    if(!isTRUE(sum(!is.na(voxdim))==3))
      stop("Cannot extract voxel dimensions from neuroglancer scene!")
  }
  if(!is.null(layers) && !all(layers %in% names(x$layers)))
    stop("Unable to find all specified layers in scene!")
  ll=ngl_layers(x, layers)
  for(i in seq_along(ll)) {
    new_name=names(layers)[i]
    newl=ll[[i]]
    if(is.null(scdims)) {
      # flywire neuroglancer scene
      newl$transform=reg
    } else {
      # regular neuroglancer scene
      if(is.null(newl$source))
        stop("No source specified in layer", names(ll)[i])
      newl$source <- set_xform_for_source(newl$source, reg, scdims)
    }
    if(is.null(new_name)) ll[[i]]=newl
    else {
      newl$name=new_name
      ll[[new_name]]=newl
    }
  }
  x$layers[names(ll)]=ll
  x
}

# private function to add transforms to different kind of ngl source blocks
# NB not 100% certain about the notes copied over from previous implementation
set_xform_for_source <- function(s, mat, outdims) {
  if(is.character(s)) {
    # NB we need an extra list wrapper if there are multiple sources
    s <- if(length(s==1)) list(url=s) else lapply(s, function(s) list(url=s))
  }
  # only want 3x4
  mat=mat[1:3, ]

  # we assume that the input matrix has nm translations
  # I thought translation must be in scaled voxel units based on scene
  # voxel dims, but instead it seems that the input objects are multiplied
  # by the matrix and then rescaled if the layer and scene dimensions don't match
  # bottom line: if we ensure scene and layer voxdims match, we don't
  # need to do anything to the supplied matrix (so long as it is in nm)
  transform=list(
    matrix=mat,
    outputDimensions=outdims)

  if(!is.null(s[['url']])) {
    # single source
    s$transform=transform
  } else {
    # multi source
    s[[1]]$transform=transform
  }
  s
}
