#' Find all merged segment ids for a given starting google segment id
#'
#' @param x a segment id or any other input that can be interpreted by
#'   \code{\link{ngl_segments}}
#' @param return.groups Whether to return the merge groups as well as the
#'   segment ids
#' @param return.segmentids.for.groups Whether to return the canonical segment
#'   id for each group rather than the 1-index of the merge group (see details).
#' @return vector of segment ids (in ascending order) or when
#'   \code{return.groups=TRUE} a \code{data.frame} with columns \itemize{
#'
#'   \item \code{segment} the integer segment id, as a numeric (double) column
#'
#'   \item \code{group} an arbitrary group id starting from 1 OR the canonical
#'   segment id (see details), an integer or numeric (double), respectively
#'
#'   }
#' @details segment ids in \code{ffn16reseg-ms1000_md0.02_c0.6_iou0.7} always
#'   match one raw segment id in \code{fafb_v14_16nm_v00c_split3xfill2} but may
#'   map to a large agglomerated merge group.
#'
#'   We provide two ways to define an identifier for each of these merge groups.
#'   One uses a canonical segment id: this is the first (i.e. lowest) segment id
#'   in the merge group and appears to be how the brainmaps API identifies the
#'   merge group. This will be the case even when there are no known merges for
#'   a given segment. In R this field will be a numeric (double) column since R
#'   does not have native support for 64 bit integers.
#'
#'   The second approach to group identifiers uses an integer from 1 to the
#'   number of known merge groups. When there are no known merges for a segment,
#'   an \code{NA} value will be returned.
#'
#'   Note that this function depends on the
#'   \code{\link[fafbsegdata]{segment_merge_groups.dt}} object from the
#'   \code{\link[fafbsegdata]{fafbsegdata}} package.
#' @export
#' @import data.table
#' @examples
#' \donttest{
#' if(requireNamespace('fafbsegdata', quietly = TRUE)) {
#' find_merged_segments(7186840767)
#' # these all belong to one merge group by definition
#' find_merged_segments(7186840767, return.groups=TRUE)
#' }
#' }
find_merged_segments <- function(x, return.groups=FALSE,
                                 return.segmentids.for.groups=TRUE) {
  if(!requireNamespace('fafbsegdata', quietly = TRUE)) {
    stop('Please do:\n',
         'devtools::install_github("jefferis/fafbsegdata")\n',
         'to install information about candidate skeleton merges')
  }
  x=ngl_segments(x)
  # to keep R CMD check quiet
  NULL->id->membership
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
    df=data.frame(segment=x, group=groups)
    if(return.segmentids.for.groups) {
      # instead of the arbitrary group numbers, we want to return the segment
      # ids for each group - this is how they are identified in the brainmaps API
      group_match=match(groups, merge_group_df[['membership']])
      df[['group']]=merge_group_df[['id']][group_match]
      df[['group']][is.na(df[['group']])]=df[['segment']][is.na(df[['group']])]
    }
    df
  } else x
}
