#' Find all merged segment ids for a given starting google segment id
#'
#' @param x a segment id or any other input that can be interpreted by
#'   \code{\link{ngl_segments}}
#' @return vector of segment ids (in ascending order)
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
#' }
find_merged_segments <- function(x) {
  if(!requireNamespace('fafbsegdata', quietly = TRUE)) {
    stop('Please do:\n',
         'devtools::install_github("jefferis/fafbsegdata")\n',
         'to install information about candidate skeleton merges')
  }
  x=ngl_segments(x)
  groups=fafbsegdata::segment_merge_groups.dt[id%in%x,][['membership']]
  groups=unique(groups)
  if(length(groups)) {
    # some segments had merge information
    merge_group_ids=fafbsegdata::segment_merge_groups.dt[membership%in%groups,][['id']]
    x <- union(x, merge_group_ids)
  }
  sort(x)
}
