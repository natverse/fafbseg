map1 <- function(xyz1, scale=2) {
  xyz1=as.integer(round(xyz1))
  baseurl <- "https://tbar.uvm.edu/app/flyconv/dataset/flywire_v1/"
  url <- sprintf(paste0(baseurl, "s/%d/z/%d/x/%d/y/%d/"),
                 scale, xyz1[3], xyz1[1], xyz1[2])
  res = httr::GET(url)
  if(httr::status_code(res)>400) {
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
  bodyj <- jsonlite::toJSON(body, auto_unbox=FALSE)
  res = httr::POST(url, body = bodyj, config = content_type_json(), encode='raw', ...)
  if(httr::status_code(res)>400) {
    warn_for_status(res)
    badval=matrix(NA_real_, ncol = 5, nrow=nrow(xyz))
    colnames(badval)=c("dx", "dy", "x", "y", "z")
    return(badval)
  }
  res = httr::content(
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
#' @param xyz A Nx3 matrix of points
#' @param method Whether to map many points at once (default) or just one
#' @param ... Additional arguments passed to httr::GET/POST operation
#'
#' @return an Nx3 matrix of points
#' @export
#'
#' @examples
#' fafb142flywirev1(cbind(163826*4, 75263*4, 3513*40))
#' fafb142flywirev1(cbind(163826*4, 75263*4, 3513*40), method="map1")
#'
#' data("AV4b1")
#' fafb142flywirev1(xyzmatrix(AV4b1)[1:50,])
fafb142flywirev1 <- function(xyz, method=c("mapmany", "map1"), chunksize=200, ...) {
  if(!isTRUE(length(dim(xyz))==2))
    stop("Please give me N x 3 points as input!")
  method=match.arg(method)
  scalefac=c(1,1,nat::voxdims(elmr::FAFB14)[3])
  scalefac=nat::voxdims(elmr::FAFB14)

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
