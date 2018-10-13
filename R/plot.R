# jet.colors <- function (n){
#     x <- ramp(seq.int(0, 1, length.out = n))
#     if (ncol(x) == 4L)
#         rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
#     else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
# }


#' Compare a neuroglancer mesh object with a regular neuron
#'
#' @param x A neuroglancer mesh, rgl::mesh3d object or other object with a
#'   defined xyzmatrix function.
#' @param n A nat::neuron object
#' @param colpal A function defining a colour palette or a vector of colour
#'   names. Should
#' @param plot Whether to plot anything (set to \code{FALSE} when you just want
#'   to get the distance information)
#' @param plotn Whether to plot the neuron \code{n}
#' @param pointsize Size of plotted points for mesh - passed on to
#'   \code{\link[rgl]{points3d}}. Default \code{pointsize=0.1} makes points
#'   smaller than usual.
#' @param sample_dots Fraction of points (0-1) from the mesh to plot - the
#'   default value of 1 implies all points. Values of \code{sample_dots < 1}
#'   select a random subsample of the points.
#' @param ... Additional arguments passed to \code{plot3d.neuron}
#' @inheritParams base::cut
#' @export
#' @return Invisibly, a data.frame with the distances of each object in \code{x}
#'   to its nearest neighbour in \code{n} as well as the breaks used for
#'   colouring points when plotting.
#'
#' @examples
#' \dontrun{
#' x=read_ng_raw('meshdata/')
#' library(elmr)
#' y=read.neuron.catmaid(23432)
#' compare_ng_neuron(x,y)
#' }
#' @importFrom rgl points3d
#' @importFrom rgl plot3d
#' @importFrom nabor knn
compare_ng_neuron <- function(x, n, breaks=3, colpal=c('cyan','red'), plot=TRUE,
                              plotn=plot, pointsize=.1, sample_dots=1, ...) {
  xyzx=xyzmatrix(x)
  if(!isTRUE(sample_dots==1)) {
    if(!is.numeric(sample_dots) || sample_dots<0 || sample_dots>1 )
      stop("sample_dots must be in range 0-1")
    nv=nrow(xyzx)
    perm=sort(sample(nv, size = floor(sample_dots*nv)))
    xyzx=xyzx[perm, , drop=FALSE]
  }
  nknn=knn(query = xyzx, data=xyzmatrix(n), k=1)
  cc=cut(nknn$nn.dists, breaks=breaks, labels=FALSE)
  if(plot){
    levels=unique(cc)
    colpal=if(is.function(colpal)) colpal(max(levels)-1) else colpal
    cols=c("grey", colpal)
    if(!all(levels) %in% seq_along(cols))
      stop("Colour palette missing some levels:", levels)

    for(i in levels) {
      points3d(xyzx[cc==i,], size=pointsize, col=cols[i])
    }
    if(plotn)
      plot3d(n, col='black', lwd=3, soma=T, skipRedraw=T, ...)
  }
  invisible(data.frame(d=nknn$nn.dists, level=cc))
}
