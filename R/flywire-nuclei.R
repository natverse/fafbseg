#' Queries for information about flywire nuclei (helpful for finding somata)
#'
#' @description \code{flywire_nuclei} finds nuclei based on known \code{rootids}
#'   or \code{nucleus_ids}
#' @inheritParams flywire_partners
#' @param nucleus_ids ids from the nucleus table to return (optional, NB only
#'   one of \code{rootids} and \code{nucleus_ids} can be provided).
#' @param rawcoords Whether to return coordinates in raw form rather than nm
#'   (default \code{FALSE})
#' @param ... Additional arguments passed to \code{\link{flywire_cave_query}}
#'
#' @return A data.frame containing information about nuclei including \itemize{
#'
#'   \item id nucleus id
#'
#'   \item pt_position the XYZ position of the centre of the nucleus (in nm)
#'
#'   \item pt_supervoxel_id corresponding to the nucleus may be missing if the
#'   segmentation is disrupted at the location of the nucleus e.g. because of
#'   masking issues, missing sections etc.
#'
#'   \item pt_root_id The current root id (when the \code{pt_position} maps onto
#'   the segmentation.)
#'
#'   \item volume the volume in cubic microns of the nucleus
#'
#'   }
#' @export
#' @importFrom dplyr right_join across
#' @importFrom nat xyzmatrix2str
#' @examples
#' \donttest{
#' # an example where there are two nucleus matches
#' flywire_nuclei(flywire_xyz2id(c(120152, 22864, 3564), rawcoords = TRUE))
#' }
flywire_nuclei <- function(rootids=NULL, nucleus_ids=NULL, rawcoords=FALSE, ...) {
  if(!is.null(rootids) & !is.null(nucleus_ids))
    stop("You must supply only one of rootids or nucleus_ids!")

  if(is.null(rootids) && is.null(nucleus_ids))
    return(flywire_cave_query(table = 'nuclei_v1', ...))
  res <- if(!is.null(rootids)) {
    rootids=flywire_ids(rootids)
    nuclei_v1 <- if(length(rootids)<200) {
      rid=paste(rootids, collapse=',')
      ridq=reticulate::py_eval(sprintf('{"pt_root_id": [%s]}', rid), convert = F)
      flywire_cave_query(table = 'nuclei_v1', filter_in_dict=ridq, ...)
    } else {
      flywire_cave_query(table = 'nuclei_v1', live = F, ...)
    }
    # bail if root id is not in table
    if(nrow(nuclei_v1)==0) return(nuclei_v1)
    nuclei_v1 <- nuclei_v1 %>%
      right_join(data.frame(pt_root_id=as.integer64(rootids)), by="pt_root_id") %>%
      select(colnames(nuclei_v1))

    if (length(rootids) < 200) {
      nuclei_v1
    } else {
      nuclei_v1 %>%
        mutate(pt_root_id = flywire_updateids(.data$pt_root_id,
                                              svids = .data$pt_supervoxel_id))
    }
  } else {
    nid=paste(nucleus_ids, collapse=',')
    nidq=reticulate::py_eval(sprintf('{"id": [%s]}', nid), convert = F)
    nuclei_v1=flywire_cave_query(table = 'nuclei_v1', filter_in_dict=nidq, ...)
    nuclei_v1 %>%
      right_join(data.frame(id=nucleus_ids), by="id") %>%
      select(colnames(nuclei_v1))
  }
  if(isFALSE(rawcoords)) res else{
    res %>%
      mutate(across(ends_with("position"),
                    function(x) xyzmatrix2str(flywire_nm2raw(x))))
  }
}


#' @description \code{flywire_nearest_nuclei} returns the nearest nucleus to a
#'   query xyz location. When \code{rawcoords=T} both the input and output
#'   positions are in raw voxels. Note however that distances are still
#'   calculated in nm. \code{xyz} may contain single points unless \code{k>1},
#'   in which case only one query point is allowed.
#'
#' @details \code{flywire_nearest_nuclei} caches the nucleus table and then
#'   updates ids of any selected values. This saves time for subsequent queries
#'   assuming that you are returning less than half of the total rows.
#'
#' @param xyz One or more (if \code{k=1}) query points. In raw coordinates when
#'   \code{rawcoords=T}
#' @param k The number of nearest nuclei to return for each query position. When
#'   \code{k>1} you are currently limited to one query point.
#'
#' @return For \code{flywire_nearest_nuclei} when \code{rawcoords=T} both the
#'   input and output positions are in raw voxels. Note however that distances
#'   are still calculated in nm.
#' @export
#' @rdname flywire_nuclei
#' @examples
#' \donttest{
#' nn=flywire_nearest_nuclei(c(480608, 91456, 142560), k=2)
#' as.data.frame(nn)
#'
#' flywire_nearest_nuclei('163113, 59074, 5295', rawcoords = T)
#' }
#'
#' \dontrun{
#' # from clipboard e.g. copied from flywire
#' flywire_nearest_nuclei(clipr::read_clip(), rawcoords = T)
#' }
flywire_nearest_nuclei <- function(xyz, rawcoords=F, k=1) {
  xyz=xyzmatrix(xyz)
  if(rawcoords) xyz=flywire_raw2nm(xyz)

  if(k>1 && nrow(xyz)>1) stop("If k>1 you can only give one point")
  nuclei_v1 <- nuclei_v1_cached()
  nnres=nabor::knn(xyzmatrix(nuclei_v1$pt_position), xyz, k=k)
  df=nuclei_v1[c(nnres$nn.idx),,drop=F]
  df$dist=c(nnres$nn.dists)
  df$pt_root_id=flywire_updateids(df$pt_root_id, svids = df$pt_supervoxel_id)
  if(isFALSE(rawcoords)) df else{
    df %>%
      mutate(across(ends_with("position"),
                    function(x) xyzmatrix2str(flywire_nm2raw(x))))
  }

}

nuclei_v1_cached <- memoise::memoise(function() flywire_cave_query(table = 'nuclei_v1', live = F), ~memoise::timeout(300))
