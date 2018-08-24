#' Convert between filenames and neuroglancer ids
#'
#' @description \code{swc2segmentid} converts an swc filename to a segment id
#'
#' @param x Input file or id
#' @param include.fragment Whether to include the sub identifier of the skeleton
#'   fragment (see details).
#' @return for \code{swc2segmentid} a numeric vector or matrix depending on the
#'   value of \code{include.fragment}
#' @export
#' @name fafbseg-ids
#' @importFrom stringr str_match
swc2segmentid <- function(x, include.fragment=FALSE) {
  res=str_match(basename(x), "^(\\d+)(\\.(\\d+)){0,1}\\.[Ss][Ww][Cc]$")
  if(isTRUE(include.fragment)) {
    res=res[,c(2,4), drop=FALSE]
    colnames(res)=c("segment", "fragment")
    res
  } else {
    res=res[,2]
  }
  mode(res)='numeric'
  res
}

#' @description \code{segmentid2zip} converts a segment id to the zip file that
#'   contains it
#' @export
#' @rdname fafbseg-ids
segmentid2zip <- function(x) {
  sprintf("%d.zip", as.numeric(x) %/% 1e5)
}

#' @description \code{zip2segmentstem} converts a zip file to the initial part
#'   of the segment id ie. the segment stem (see details).
#'
#' @details Segment ids are unique integers. There are about 8E8 in the current
#'   skeletonisation but it seems that the ids can still be > 2^31 (usually
#'   \code{.Machine$integer.max}). Therefore they will be stored in R as numeric
#'   values or the \code{bit64::integer64} values.
#'
#'   Each segmentation has keen skeletonised however this usually results in
#'   multiple skeleton fragments which have been written out as separate SWC
#'   files: \code{"named <segment id>.<fragment>.swc"}
#'
#'   Each segment id is mapped onto a zip file by dividing by 1e5 and discarding
#'   the remainder.
#' @export
#' @rdname fafbseg-ids
#' @importFrom tools file_path_sans_ext
zip2segmentstem <- function(x) {
  as.integer(file_path_sans_ext(basename(x)))
}
