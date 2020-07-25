
#' @importFrom httr GET POST status_code warn_for_status content content_type_json
#' @importFrom jsonlite toJSON
map1 <- function(xyz1, scale=2) {
  xyz1=as.integer(round(xyz1))
  baseurl <- "https://spine.janelia.org/app/flyconv/dataset/flywire_v1/"
  url <- sprintf(paste0(baseurl, "s/%d/z/%d/x/%d/y/%d/"),
                 scale, xyz1[3], xyz1[1], xyz1[2])
  res = GET(url)
  if(status_code(res)>400) {
    warn_for_status(res)
    badval=rep(NA, 5)
    names(badval)=c("dx", "dy", "x", "y", "z")
    return(badval)
  }
  res = httr::content(
    res,
    as = 'parsed',
    type = 'application/json',
    encoding = 'utf-8',
    simplifyVector = TRUE
  )
  unlist(res)
}

#' @importFrom httr content_type
mapmany <- function(xyz, scale=2, usemsgpack=NULL, ...) {
  if(!is.matrix(xyz) || ncol(xyz)!=3)
    stop("I need an Nx3 matrix of points!")
  xyz=round(xyz)
  baseurl <- "https://spine.janelia.org/app/flyconv/dataset/flywire_v1"
  url <- sprintf("%s/s/%d/values", baseurl, scale)
  body <- list(locations=xyz)
  if(is.null(usemsgpack))
    usemsgpack <- requireNamespace('RcppMsgPack', quietly = TRUE)
  resp <- if(usemsgpack) {
    bodym=RcppMsgPack::msgpack_pack(body)
    POST(url, body = bodym, config = content_type("application/msgpack"),
         encode='raw', ...)
  } else {
    bodyj <- toJSON(body, auto_unbox=FALSE)
    POST(url, body = bodyj, config = content_type_json(), encode='raw', ...)
  }
  if(status_code(resp)>400) {
    warn_for_status(resp)
    badval=matrix(NA_real_, ncol = 5, nrow=nrow(xyz))
    colnames(badval)=c("dx", "dy", "x", "y", "z")
    return(badval)
  }
  if(usemsgpack) {
    rawres=content(resp, as='raw',type = 'application/msgpack')
    RcppMsgPack::msgpack_unpack(rawres, simplify = T)
  } else {
    strres = content(
      resp,
      as = 'text',
      type = 'application/json',
      encoding = 'utf-8',
    )
    strres=gsub("NaN", '"NA"', strres, fixed = TRUE)
    jsonlite::fromJSON(strres, simplifyVector = TRUE)
  }

}

#' Map points in  FlyWire v1 TO FAFB14 space (xyz nm)
#'
#' @details Note that you can also access FlyWire->FAFB bridging registration
#'   via the \code{\link{xform_brain}} series of functions. This will allow you
#'   to transform most kinds of 3D data objects, whereas the \code{flywire2fafb}
#'   function is restricted to plain 3D coordinates. See examples.
#'
#'   Mapping single points is unlikely to be useful, but you may wish to adjust
#'   the \code{chunksize} argument to send more points at once at the risk of
#'   possible server timeouts. The value of 200 is quite conservative.
#'
#'   When \code{swap=TRUE} displacements will be applied in the opposite
#'   direction to what is intended. This can be used to provide a coarse
#'   FAFB->FlyWire mapping if you feed in FAFB points. This is wrong but may be
#'   useful as it can get you closer to the right place in FlyWire than just
#'   assuming that FAFB14 and FlyWire are in the same space. This works because
#'   deformations are mostly fairly smooth at the scale of FAFB-FlyWire
#'   displacements. Operationally we find that residual displacements are
#'   typically of the order 100 nm using this procedure.
#'
#' @param xyz A Nx3 matrix of points
#' @param method Whether to map many points at once (default) or just one
#' @param chunksize The number of points to send to the server when mapping many
#'   points at once.
#' @param swap When \code{TRUE} applies the deformation field in the opposite
#'   direction e.g. to give a coarse mapping of points FAFB->FlyWire. This is
#'   wrong but may be useful.
#' @param ... Additional arguments passed to httr::GET/POST operation
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
#' # check displacement for "fake inverse"
#' # NB this is not a proper FAFB->FlyWire mapping  (see details)
#' flywire2fafb(p.fafb.nm, swap=TRUE)-p.flywire.nm
#'
#' data("AV4b1", package='catmaid')
#' set.seed(42)
#' before=xyzmatrix(AV4b1)[sample(nvertices(AV4b1), size=2000), ]
#' after=flywire2fafb(before, swap=TRUE)
#' d=sqrt(rowSums((before-after)^2))
#' hist(d, br=20)
#'
#' \dontrun{
#' AV4b1.flywire <- xform_brain(AV4b1, reference="FlyWire", sample="FAFB14")
#' plot3d(neuronlist(AV4b1.flywire, AV4b1))
#' }
flywire2fafb <- function(xyz, method=c("mapmany", "map1"), chunksize=200,
                         swap=FALSE, ...) {
  if(!isTRUE(length(dim(xyz))==2))
    stop("Please give me N x 3 points as input!")
  method=match.arg(method)
  if(swap)
    warn_hourly("The FAFB->FlyWire transform is wrong but useful. See ?flywire2fafb")
  # hard code to avoid elmr dependency just for this
  # scalefac=nat::voxdims(elmr::FAFB14)
  scalefac=c(4, 4, 40)

  xyzraw=scale(xyz, center=FALSE, scalefac)
  if(method=='map1')
    mapres=t(pbapply::pbapply(xyzraw, 1, map1, ...))
  else {
    nx=nrow(xyz)
    nchunks=ceiling(nx/chunksize)
    if(nchunks==1) {
      # only 1 chunk, let's keep this simple
      mapres=mapmany(xyzraw, ...)
    } else {
      # multiple chunks
      chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nx)]
      chunkstoread=seq_len(nchunks)
      b=by(xyzraw, chunks, as.matrix)
      l=t(pbapply::pblapply(b, mapmany, ...))
      mapres=dplyr::bind_rows(l)
    }
  }
  # let's get the xy deltas; dz is always 0
  deltas=cbind(mapres[,c("dx", "dy"), drop=F], 0)
  xyzrawt <- if(swap) xyzraw-deltas else xyzraw+deltas
  xyzt=scale(xyzrawt, center=FALSE, 1/scalefac)
  xyzt[is.na(xyzt)]=NA_real_
  # tidy up attributes
  rownames(xyzt) <- NULL
  colnames(xyzt) <- c("X", "Y", "Z")
  attr(xyzt, "scaled:scale") <- NULL
  xyzt
}

warn_hourly <-
  memoise::memoise(function(..., call. = FALSE, immediate. = TRUE)
    warning(..., call. = call., immediate. = immediate.),
    ~ memoise::timeout(3600))

# Private function to make bridging registration available to xform and friends
register_fafb_flywire <- function() {
  flywire2fafb.reg <- nat::reglist(function(xyz, ...) flywire2fafb(xyz, ...))
  fafb2flywire.reg <- nat::reglist(function(xyz, ...) flywire2fafb(xyz, swap=TRUE, ...))
  nat.templatebrains::add_reglist(flywire2fafb.reg, sample = 'FlyWire',
                                  reference='FAFB14')
  nat.templatebrains::add_reglist(fafb2flywire.reg, reference = 'FlyWire',
                                  sample='FAFB14')
}
