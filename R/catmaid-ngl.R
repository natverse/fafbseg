# Convert CATMAID things (eg neurons) into Neuroglancer equivalents
#' @export
catmaid2ngl <- function(x, ...) UseMethod('catmaid2ngl')

#' @export
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
#' @importFrom nat nlapply
catmaid2ngl.neuronlist <- function(x, OmitFailures=TRUE, ...) {
  nlapply(x, catmaid2ngl, OmitFailures=OmitFailures, ...)
}

#' @export
catmaid2ngl.character <- function(x, open=FALSE, ...) {
  open_fafb_ngl(x, open=open, ...)
}

#' @export
#' @importFrom catmaid read.neuron.catmaid catmaid_skids
catmaid2ngl.default <- function(x, ...) {
  x=read.neuron.catmaid(catmaid_skids(x, several.ok=FALSE, ...), ...)
  catmaid2ngl(x)
}
