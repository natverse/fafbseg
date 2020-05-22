
#' @importFrom httr GET POST status_code warn_for_status content content_type_json
#' @importFrom jsonlite toJSON
map1 <- function(xyz1, scale=2) {
  xyz1=as.integer(round(xyz1))
  baseurl <- "https://tbar.uvm.edu/app/flyconv/dataset/flywire_v1/"
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

mapmany <- function(xyz, scale=2, ...) {
  if(!is.matrix(xyz) || ncol(xyz)!=3)
    stop("I need an Nx3 matrix of points!")
  xyz=round(xyz)
  baseurl <- "https://tbar.uvm.edu/app/flyconv/dataset/flywire_v1"
  url <- sprintf("%s/s/%d/values", baseurl, scale)
  body <- list(locations=xyz)
  bodyj <- toJSON(body, auto_unbox=FALSE)
  res = POST(url, body = bodyj, config = content_type_json(), encode='raw', ...)
  if(status_code(res)>400) {
    warn_for_status(res)
    badval=matrix(NA_real_, ncol = 5, nrow=nrow(xyz))
    colnames(badval)=c("dx", "dy", "x", "y", "z")
    return(badval)
  }
  res = content(
    res,
    as = 'parsed',
    type = 'application/json',
    encoding = 'utf-8',
    simplifyVector = TRUE
  )
  res
}

#' Map points in FAFB14 space (xyz nm) to FlyWire v1
#'
#' @details Note that you can also access FAFB->FlyWire bridging registration
#'   via the \code{\link{xform_brain}} series of functions. This will allow you
#'   to transform most kinds of 3D data objects, whereas the \code{fafb2flywire}
#'   function is restricted to plain 3D coordinates. See examples.
#'
#'   Mapping single points is unlikely to be useful, but you may wish to adjust
#'   the \code{chunksize} argument to send more points at once at the risk of
#'   possible server timeouts. The value of 200 is quite conservative.
#'
#' @param xyz A Nx3 matrix of points
#' @param method Whether to map many points at once (default) or just one
#' @param chunksize The number of points to send to the server when mapping many
#'   points at once.
#' @param ... Additional arguments passed to httr::GET/POST operation
#'
#' @return an Nx3 matrix of points
#' @export
#'
#' @examples
#' fafb2flywire(cbind(163826*4, 75263*4, 3513*40))
#' fafb2flywire(cbind(163826*4, 75263*4, 3513*40), method="map1")
#'
#' data("AV4b1", package='catmaid')
#' set.seed(42)
#' before=xyzmatrix(AV4b1)[sample(nvertices(AV4b1), size=2000), ]
#' after=fafb2flywire(before)
#' d=sqrt(rowSums((before-after)^2))
#' hist(d, br=20)
#'
#' \dontrun{
#' AV4b1.flywire <- xform_brain(AV4b1, reference="FlyWire", sample="FAFB14")
#' plot3d(neuronlist(AV4b1.flywire, AV4b1))
#' }
fafb2flywire <- function(xyz, method=c("mapmany", "map1"), chunksize=200,
                             ...) {
  if(!isTRUE(length(dim(xyz))==2))
    stop("Please give me N x 3 points as input!")
  method=match.arg(method)
  # hard code to avoid elmr dependency just for this
  # scalefac=nat::voxdims(elmr::FAFB14)
  scalefac=c(4, 4, 40)

  xyzraw=scale(xyz, center=FALSE, scalefac)
  if(method=='map1')
    xyzrawt=t(pbapply::pbapply(xyzraw, 1, map1, ...))
  else {
    nx=nrow(xyz)
    nchunks=ceiling(nx/chunksize)
    if(nchunks==1) {
      # only 1 chunk, let's keep this simple
      xyzrawt=mapmany(xyzraw, ...)
    } else {
      # multiple chunks
      chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nx)]
      chunkstoread=seq_len(nchunks)
      b=by(xyzraw, chunks, as.matrix)
      l=t(pbapply::pblapply(b, mapmany, ...))
      xyzrawt=dplyr::bind_rows(l)
    }
  }
  xyzt=scale(xyzmatrix(xyzrawt), center=FALSE, 1/scalefac)
  xyzt[is.na(xyzt)]=NA_real_
  # tidy up attributes
  rownames(xyzt) <- NULL
  attr(xyzt, "scaled:scale") <- NULL
  xyzt
}

# Private function to make bridging registration available to xform and friends
register_fafb_flywire <- function() {
  fafb2flywire.reg <- nat::reglist(function(xyz, ...) fafb2flywire(xyz, ...))
  nat.templatebrains::add_reglist(fafb2flywire.reg, reference = 'FlyWire',
                                  sample='FAFB14')
}
