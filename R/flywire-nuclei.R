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
#'   \item pt_position the XYZ position of the centre of the nucleus. This will
#'   always be in nm when \code{rawcoords=FALSE} even if the remote table stores
#'   raw (uncalibrated) voxel coordinates. It will be a comma separated string
#'   when rawcoords=TRUE since this is most convenient for pasting between
#'   applications.
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
    return(standard_nuclei(flywire_cave_query(table = nucleus_table_name(), ...)))
  res <- if(!is.null(rootids)) {
    rootids=flywire_ids(rootids)
    nuclei <- if(length(rootids)<200) {
      rid=paste(rootids, collapse=',')
      ridq=reticulate::py_eval(sprintf('{"pt_root_id": [%s]}', rid), convert = F)
      flywire_cave_query(table = nucleus_table_name(), filter_in_dict=ridq, ...)
    } else {
      flywire_cave_query(table = nucleus_table_name(), live = F, ...)
    }
    # bail if root id is not in table
    if(nrow(nuclei)==0) return(nuclei)
    nuclei <- nuclei %>%
      right_join(data.frame(pt_root_id=as.integer64(rootids)), by="pt_root_id") %>%
      select(colnames(nuclei))

    if (length(rootids) < 200) {
      nuclei
    } else {
      nuclei %>%
        mutate(pt_root_id = flywire_updateids(.data$pt_root_id,
                                              svids = .data$pt_supervoxel_id))
    }
  } else {
    nid=paste(nucleus_ids, collapse=',')
    nidq=reticulate::py_eval(sprintf('{"id": [%s]}', nid), convert = F)
    nuclei=flywire_cave_query(table = nucleus_table_name(), filter_in_dict=nidq, ...)
    nuclei %>%
      right_join(data.frame(id=nucleus_ids), by="id") %>%
      select(colnames(nuclei))
  }
  res <- standard_nuclei(res)
  if(isFALSE(rawcoords)) res else {
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
  nuclei <- cached_nuclei()
  nnres=nabor::knn(xyzmatrix(nuclei$pt_position), xyz, k=k)
  df=nuclei[c(nnres$nn.idx),,drop=F]
  df$dist=c(nnres$nn.dists)
  df$pt_root_id=flywire_updateids(df$pt_root_id, svids = df$pt_supervoxel_id)
  if(isFALSE(rawcoords)) df else{
    df %>%
      mutate(across(ends_with("position"),
                    function(x) xyzmatrix2str(flywire_nm2raw(x))))
  }

}

# private function to ensure that we always have coords in nm
standard_nuclei <- function(df, datastack_name=getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  vd=nucleus_table_info(datastack_name = datastack_name)[['voxel_resolution']]
  isnm=is.null(vd) || isTRUE(all.equal(vd,c(1,1,1)))
  if(isnm) return(df)
  # otherwise we need to scale up by the voxel dimensions in the table
  df %>%
    mutate(across(ends_with("position"),
                  function(x) xyzmatrix2list(flywire_raw2nm(x, vd=vd))))
}

# private function to cache nucleus table for given dataset
cached_nuclei <- memoise::memoise(function(datastack_name=getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  table=nucleus_table_name(datastack_name)
  df=flywire_cave_query(table = table, live = T)
  standard_nuclei(df)
}, ~memoise::timeout(3600))

# try to find the nucleus table in a consistent way so that we can use
# these functions for FANC as well
# see https://fanc-reconstruction.slack.com/archives/C0133P1GRV0/p1642233199005400
# and https://flywire-forum.slack.com/archives/C01M4LP2Y2D/p1642237506018900
# for some relevant discussion
nucleus_table_name <- memoise::memoise(function(datastack_name=getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  fac=flywire_cave_client(datastack_name=datastack_name)
  dsinfo=fac$info$get_datastack_info()
  if(!is.null(dsinfo$soma_table))
    return(dsinfo$soma_table)
  # do we have a table nuclei_v1 which we will hard code as preferred for now?
  tt=fac$annotation$get_tables()
  nucleus_tables <- grep("^nuclei_", tt)
  if(length(nucleus_tables)==0)
    stop("I cannot find a nucleus table for datastack: ", datastack_name,
         "\nPlease ask for help on #annotation_infrastructure https://flywire-forum.slack.com/archives/C01M4LP2Y2D")
  if(length(nucleus_tables)==1)
    return(tt[nucleus_tables])
  chosen=tt[rev(nucleus_tables)[1]]
  warning("Multiple candidate nucleus tables. Choosing: ", chosen)
  return(chosen)
}, ~memoise::timeout(60^2))


# nb we include datastack_name as an arg so it is part of the memoisation hash
# in case we ever have two datasets with a table with the same name
nucleus_table_info <- memoise::memoise(function(datastack_name=getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")) {
  fac=flywire_cave_client(datastack_name=datastack_name)
  table=nucleus_table_name(datastack_name = datastack_name)
  info=fac$annotation$get_table_metadata(table)
  info
})
