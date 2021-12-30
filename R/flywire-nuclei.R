#' Queries for information about flywire nuclei (helpful for finding somata)
#'
#' @inheritParams flywire_partners
#' @param nucleus_ids ids from the nucleus table to return (optional, NB only
#'   one of \code{rootids} and \code{nucleus_ids} can be provided).
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
#' @importFrom dplyr right_join
#' @examples
#' \donttest{
#' # an example where there are two nucleus matches
#' flywire_nuclei(flywire_xyz2id(c(120152, 22864, 3564), rawcoords = TRUE))
#' }
flywire_nuclei <- function(rootids=NULL, nucleus_ids=NULL, ...) {
  if(!is.null(rootids) & !is.null(nucleus_ids))
    stop("You must supply only one of rootids or nucleus_ids!")

  if(is.null(rootids) && is.null(nucleus_ids))
    return(flywire_cave_query(table = 'nuclei_v1', ...))
  if(!is.null(rootids)) {
    rootids=flywire_ids(rootids)
    nuclei_v1 <- if(length(rootids)<200) {
      rid=paste(rootids, collapse=',')
      ridq=reticulate::py_eval(sprintf('{"pt_root_id": [%s]}', rid), convert = F)
      flywire_cave_query(table = 'nuclei_v1', filter_in_dict=ridq, ...)
    } else {
      flywire_cave_query(table = 'nuclei_v1', live = F, ...)
    }
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
}

flywire_nearest_nuclei <- function(xyz, rawcoords=FALSE, voxdims = c(4, 4, 40)) {
  if(isTRUE(is.numeric(xyz) && is.vector(xyz) && length(xyz)==3)) {
    xyz=matrix(xyz, ncol=3)
  } else {
    xyz=xyzmatrix(xyz)
  }
  if(isTRUE(rawcoords)) {
    xyz <- scale(xyz, scale = 1/voxdims, center = FALSE)
  }
  nuclei_v1 <- flywire_cave_query(table = 'nuclei_v1', live = F)
  nnres=nabor::knn(xyzmatrix(nuclei_v1$pt_position), xyz, k=1)
  df=nuclei_v1[nnres$nn.idx,,drop=F]
  df$dist=c(nnres$nn.dists)
  df$pt_root_id=flywire_updateids(df$pt_root_id, svids = df$pt_supervoxel_id)
  df
}
