#' @importFrom httr GET POST status_code warn_for_status content content_type_json
#' @importFrom jsonlite toJSON
map1 <- function(xyz1, scale=2, baseurl) {
  xyz1=as.integer(round(xyz1))
  url <- file.path(baseurl,
                   sprintf("s/%d/z/%d/x/%d/y/%d/",
                           scale, xyz1[3], xyz1[1], xyz1[2]))
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
mapmany <- function(xyz, scale=2, msgpack=FALSE, round=TRUE, baseurl, ...) {
  if(!is.matrix(xyz) || ncol(xyz)!=3)
    stop("I need an Nx3 matrix of points!")
  xyz=round(xyz)
  # because we should be rounding to nearest voxel I think
  # maybe check this with Eric Perlman. Should def be case for z.
  if(round)
    mode(xyz)='integer'
  url <- sprintf("%s/s/%d/values_array", baseurl, scale)
  if(is.null(msgpack))
    msgpack <- requireNamespace('RcppMsgPack', quietly = TRUE)
  body <- list(x=xyz[,1], y=xyz[,2], z=xyz[,3])
  # msgpack doesn't handle length 1 arrays
  msgpack=msgpack && length(body$x)>1
  resp <- if(msgpack) {
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
  res <- if(msgpack) {
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
  cols=do.call(cbind, res)
  cn=colnames(cols)
  desiredcols=c("dx", "dy", "x", "y", "z")
  if(!is.null(colnames) && all(desiredcols %in% cn)) {
    cols=cols[,desiredcols, drop=FALSE]
  } else {
    colnames(cols)=desiredcols
  }
  cols
}

mapwrapper <- function(xyz, baseurl, method, swap, chunksize, ...) {

  if(!isTRUE(length(dim(xyz))==2))
    stop("Please give me N x 3 points as input!")

  # hard code to avoid elmr dependency just for this
  # scalefac=nat::voxdims(elmr::FAFB14)
  scalefac=c(4, 4, 40)

  xyzraw=scale(xyz, center=FALSE, scalefac)
  if(method=='map1')
    mapres=t(pbapply::pbapply(xyzraw, 1, map1, baseurl = baseurl, ...))
  else {
    nx=nrow(xyz)
    nchunks=ceiling(nx/chunksize)
    if(nchunks==1) {
      # only 1 chunk, let's keep this simple
      mapres=mapmany(xyzraw, baseurl = baseurl, ...)
    } else {
      # multiple chunks
      chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nx)]
      chunkstoread=seq_len(nchunks)
      b=by(xyzraw, chunks, as.matrix)
      l=t(pbapply::pblapply(b, mapmany, baseurl = baseurl, ...))
      mapres=do.call(rbind, l)
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
