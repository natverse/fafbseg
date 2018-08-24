#' Extract the XYZ coordinates from neuroglancer mesh objects
#'
#' @param x an ng_raw or ng_raw_list object
#' @param ... additional arguments (currently ignored)
#' @export
#' @importFrom nat xyzmatrix
#' @rdname xyzmatrix
#' @aliases xyzmatrix
#' @seealso \code{\link[nat]{xyzmatrix}}
xyzmatrix.ng_raw <- function(x, ...) {
  x$coords
}

#' @export
#' @importFrom nat xyzmatrix
#' @rdname xyzmatrix
xyzmatrix.ng_raw_list <- function(x, ...) {
  emptymat=matrix(nrow=0, ncol=3, dimnames = list(NULL, c("X","Y","Z")))
  if(!length(x)) return(emptymat)
  do.call(rbind, lapply(x, xyzmatrix))
}

## @importFrom rgl as.mesh3d
## @name as.mesh3d
## @export
## as.mesh3d <- rgl::as.mesh3d

#' Convert a raw neuroglancer mesh into a mesh3d object
#'
#' @param x An ng_raw or ng_raw_list object
#' @param ... additional arguments passed to \code{\link{tmesh3d}}
#' @rdname as.mesh3d
#' @importFrom rgl as.mesh3d
#' @export
as.mesh3d.ng_raw <- function(x, ...) {
  rgl::tmesh3d(vertices = t(x$coords), indices = t(x$indices)+1, homogeneous = F, ...)
}

#' @export
#' @rdname as.mesh3d
as.mesh3d.ng_raw_list <- function(x, ...) {
  y=unlist(x)
  as.mesh3d(y)
}

#' @export
#' @method unlist ng_raw_list
unlist.ng_raw_list <- function(x, ...) {
  rawindexlist=lapply(x, function(y) y$indices)

  nrows=sapply(x, function(y) nrow(y$coords))
  # we need to make an offset to the indices
  offsets=c(0,cumsum(nrows[-length(nrows)]))
  corindices=mapply('+', rawindexlist, offsets, SIMPLIFY = F)
  res=list(coords=xyzmatrix(x),
           indices=do.call(rbind, corindices))
  class(res)='ng_raw'
  res
}
