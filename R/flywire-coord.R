#' Map points between FlyWire v1 and FAFB14 space (xyz nm)
#'
#' @description \code{flywire2fafb} maps points FlyWire->FAFB
#'
#' @details Note that you can also access FlyWire<->FAFB bridging registrations
#'   via the \code{\link{xform_brain}} series of functions. This will allow you
#'   to transform most kinds of 3D data objects, whereas the \code{flywire2fafb}
#'   function is restricted to plain 3D coordinates. See examples.
#'
#'   Mapping single points is unlikely to be useful, but you may wish to adjust
#'   the \code{chunksize} argument to send more points at once at the risk of
#'   possible server timeouts. The default value is quite conservative.
#'
#'   When \code{swap=TRUE} displacements will be applied in the opposite
#'   direction to what is intended. This can be used to provide a coarse inverse
#'   mapping if you feed in FAFB points. This is wrong but was useful before the
#'   inverse mapping was available as it can get you closer to the right place
#'   in FlyWire than just assuming that FAFB14 and FlyWire are in the same
#'   space. This works because deformations are mostly fairly smooth at the
#'   scale of FAFB-FlyWire displacements. Operationally we find that residual
#'   displacements are typically of the order 100 nm using this procedure. Since
#'   2020-08-08 we have a real inverse available, so this is now only of
#'   historical interest.
#'
#' @param xyz A Nx3 matrix of points
#' @param method Whether to map many points at once (default) or just one
#' @param chunksize The number of points to send to the server when mapping many
#'   points at once.
#' @param swap When \code{TRUE} applies the deformation field in the opposite
#'   direction e.g. to give a coarse mapping of points FAFB->FlyWire. This is
#'   wrong but may be useful.
#' @param ... Additional arguments passed to \code{httr::GET}/\code{POST}
#'   operation
#'
#' @return an Nx3 matrix of points
#' @export
#'
#' @examples
#' # identified location in FAFB14
#' p.fafb.nm <- cbind(477042, 284535, 90680)
#' p.fafb.raw <- p.fafb.nm/c(4,4,40)
#' # corresponding location in FlyWire
#' p.flywire.raw <- cbind(118865, 71338, 2267)
#' p.flywire.nm <- p.flywire.raw * c(4,4,40)
#'
#' # check displacement
#' flywire2fafb(p.flywire.nm)-p.fafb.nm
#'
#' # check what happens when you apply the inverse
#' fafb2flywire(p.fafb.nm)-p.flywire.nm
#'
#' data("AV4b1", package='catmaid')
#' set.seed(42)
#' before=xyzmatrix(AV4b1)[sample(nvertices(AV4b1), size=2000), ]
#' after=fafb2flywire(before)
#' d=sqrt(rowSums((before-after)^2))
#' hist(d, br=20, main="FAFB14 - FlyWire Displacements", xlab="d /nm")
#'
#' \dontrun{
#' AV4b1.flywire <- xform_brain(AV4b1, reference="FlyWire", sample="FAFB14")
#' plot3d(neuronlist(AV4b1.flywire, AV4b1))
#' }
flywire2fafb <- function(xyz, method=c("mapmany", "map1"), chunksize=40e3,
                         swap=FALSE, ...) {
  method=match.arg(method)
  if(swap)
    warn_hourly("Please use fafb2flywire for more accurate FAFB->FlyWire transforms. See ?flywire2fafb")

  baseurl <- "https://spine.janelia.org/app/transform-service/dataset/flywire_v1"
  mapwrapper(xyz, baseurl=baseurl, method=method, chunksize=chunksize, swap=swap, ...)
}

#' @rdname flywire2fafb
#' @export
#' @description \code{fafb2flywire} maps points FAFB->FlyWire
fafb2flywire <- function(xyz, method=c("mapmany", "map1"), chunksize=40e3, swap=FALSE, ...) {
  method=match.arg(method)
  if(swap)
    warn_hourly("Please use flywire2fafb for more accurate FlyWire->FAFB transforms. See ?flywire2fafb")

  baseurl <- "https://spine.janelia.org/app/transform-service/dataset/flywire_v1_inverse"
  mapwrapper(xyz, baseurl=baseurl, method=method, chunksize=chunksize, swap=swap, scale=4, ...)
}


# Private function to make bridging registration available to xform and friends
register_fafb_flywire <- function() {
  flywire2fafb.reg <- nat::reglist(function(xyz, ...) flywire2fafb(xyz, ...))
  fafb2flywire.reg <- nat::reglist(function(xyz, ...) fafb2flywire(xyz, ...))
  nat.templatebrains::add_reglist(flywire2fafb.reg, sample = 'FlyWire',
                                  reference='FAFB14')
  nat.templatebrains::add_reglist(fafb2flywire.reg, reference = 'FlyWire',
                                  sample='FAFB14')
}
