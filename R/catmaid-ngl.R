#' Convert CATMAID things (eg neurons) into Neuroglancer equivalents
#' @export
#' @param x An object to convert (see Descriptions for each method)
#' @param ... Additional arguments passed to methods
catmaid2ngl <- function(x, ...) UseMethod('catmaid2ngl')

#' @export
#' @rdname catmaid2ngl
#' @description \code{catmaid2ngl.neuron} uses \code{\link{brainmaps_xyz2id}}
#'   and \code{\link{read_segments2}} to find the skeletons corresponding to a
#'   (catmaid) neuron
catmaid2ngl.neuron <- function(x, ...) {
  # readfun=c("read_segments2", "read.neuron.brainmaps")
  df = as.data.frame(xyzmatrix(x))
  ids = brainmaps_xyz2id(df)
  uids = setdiff(ids, 0)
  res=read_segments2(uids, ...)
  # still could be useful but merged segments not well supported just now
  # ffids = find_merged_segments(uids, return.groups = TRUE)
  # x = FUN(ffids$segment, ...)
  # m = merge(x[, ], ffids)
  # x[, ] = m
  # df = cbind(df, segment = ids)
  # df = merge(df, m, all.x = TRUE)
  # attr(x, 'idmap') = df
  res
}

#' @export
#' @rdname catmaid2ngl
#' @importFrom nat nlapply
#' @inheritParams nat::nlapply
#' @description \code{catmaid2ngl.neuronlist} applies \code{catmaid2ngl.neuron}
#'   over a \code{\link{neuronlist}}
catmaid2ngl.neuronlist <- function(x, OmitFailures=TRUE, ...) {
  nlapply(x, catmaid2ngl, OmitFailures=OmitFailures, ...)
}

#' @rdname catmaid2ngl
#' @export
#' @inheritParams open_fafb_ngl
#' @description \code{catmaid2ngl.character} uses \code{\link{open_fafb_ngl}} to
#'   open CATMAID at a location matching a braingazer URL.
catmaid2ngl.character <- function(x, open=FALSE, ...) {
  open_fafb_ngl(x, open=open, ...)
}

#' @rdname catmaid2ngl
#' @export
#' @importFrom catmaid read.neuron.catmaid catmaid_skids
#' @description Converts a CATMAID skid specification (see
#'   \code{\link{catmaid_skids}}) for one neuron into a an autosegmentation
#'   based neuron using \code{catmaid2ngl.neuron}.
catmaid2ngl.default <- function(x, ...) {
  x=read.neuron.catmaid(catmaid_skids(x, several.ok=FALSE, ...), ...)
  catmaid2ngl(x)
}
