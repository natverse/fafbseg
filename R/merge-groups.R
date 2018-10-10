#' Find all merged segment ids for a given starting google segment id
#'
#' @param x a segment id or any other input that can be interpreted by
#'   \code{\link{ngl_segments}}
#' @param return.groups Whether to return the merge groups as well as the
#'   segment ids
#' @return vector of segment ids (in ascending order) or when
#'   \code{return.groups=TRUE} a \code{data.frame} with columns \itemize{
#'
#'   \item{segment}{ the integer segment id}
#'
#'   \item{group}{ an arbitrary group id starting from 1}
#'
#'   }
#' @details segment ids in \code{ffn16reseg-ms1000_md0.02_c0.6_iou0.7} always
#'   match one raw segment id in \code{fafb_v14_16nm_v00c_split3xfill2} but may
#'   map to a large agglomerated merge group.
#'
#'   Note that this functions depends on the
#'   \code{\link[fafbsegdata]{segment_merge_groups.dt}} object from the
#'   \code{\link[fafbsegdata]{fafbsegdata}} package.
#' @export
#' @import data.table
#' @examples
#' \donttest{
#' find_merged_segments(7186840767)
#' # these all belong to one merge group by definition
#' find_merged_segments(7186840767, return.groups=TRUE)
#' }
find_merged_segments <- function(x, return.groups=FALSE) {
  if(!requireNamespace('fafbsegdata', quietly = TRUE)) {
    stop('Please do:\n',
         'devtools::install_github("jefferis/fafbsegdata")\n',
         'to install information about candidate skeleton merges')
  }
  x=ngl_segments(x)
  groups=fafbsegdata::segment_merge_groups.dt[id%in%x,][['membership']]
  ugroups=unique(groups)
  if(length(ugroups)) {
    # some segments had merge information
    merge_group_df=fafbsegdata::segment_merge_groups.dt[membership%in%ugroups,]
    merge_group_ids=merge_group_df[['id']]
    x <- union(x, merge_group_ids)
  }
  x=sort(x)
  if(return.groups) {
    m=match(x, merge_group_df[['id']])
    groups=merge_group_df[['membership']][m]
    data.frame(segment=x, group=groups)
  } else x
}
