#' Summarise edits for one or more FlyWire segment ids
#'
#' @details When \code{filtered=TRUE} the following are removed \itemize{
#'
#'   \item edits that are outside of the given root id - meaning these edits are
#'   in the history of the cell but the affected part of the arbour was since
#'   cut off.
#'
#'   \item edits that were since undone by an inverse operation (often temporary
#'   splits made while reviewing a neuron.)
#'
#'   }
#'
#' @param x One or more flywire segment ids (character vector strongly
#'   recommended but any format accepted by \code{\link{ngl_segments}} will
#'   work).
#' @param root_ids Whether to look up the root ids before/after each change.
#'   Default \code{FALSE}. NB this is quite resource intensive so please do not
#'   flood the server with requests of this sort.
#' @param filtered Whether to filter out edits unlikely to relate to the current
#'   state of the neuron (default \code{TRUE}, see details).
#' @param tz Time zone for edit timestamps. Defaults to "UTC" i.e. Universal
#'   Time, Coordinated. Set to "" for your current timezone. See
#'   \code{\link{as.POSIXct}} for more details.
#' @param ... Additional arguments passed to \code{\link{flywire_fetch}}
#'
#' @return A data frame with values itemize{
#'
#'   \item{operation_id}{ a unique id for the edit}
#'
#'   \item{timestamp}{ in POSIXct format, to the nearest ms}
#'
#'   \item{user_id}{ numeric id for the user responsible for the edit}
#'
#'   \item{is_merge}{ whether it was a merge or a split}
#'
#'   \item{user_name}{ as a string}
#'
#'   }
#'
#'   In addition when \code{filtered=FALSE}, \code{in_neuron} \code{is_relevant}
#'
#'   In addition when \code{root_ids=TRUE}, \code{before_root_ids}
#'   \code{after_root_ids}, as space separated strings.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flywire_change_log("720575940619010932")
#' flywire_change_log("720575940619010932", root_ids = TRUE)
#' flywire_change_log("720575940619010932", filtered = FALSE)
#' }
flywire_change_log <- function(x, root_ids=FALSE, filtered=TRUE, tz="UTC", ...) {
  x=flywire_segments(x)
  if(length(x)>1) {
    res=pbapply::pbsapply(x, flywire_change_log, ..., simplify = FALSE)
    return(dplyr::bind_rows(res, .id='id'))
  }

  pu=httr::parse_url("https://prodv1.flywire-daf.com/segmentation/api/v1/table/fly_v31/root/%s/tabular_change_log")
  pu
  pu$path=sprintf(pu$path, x)
  pu$query=list(root_ids=as.character(root_ids),
                filtered=as.character(filtered))
  url=httr::build_url(pu)
  res=flywire_fetch(url, ...)
  if(isTRUE(root_ids)) {
    res[['before_root_ids']]=sapply(res[['before_root_ids']], paste,
                                    collapse=" ", USE.NAMES = F)
    res[['after_root_ids']]=sapply(res[['after_root_ids']], paste,
                                   collapse=" ", USE.NAMES = F)
  }
  df=as.data.frame(lapply(res, unlist))
  df$user_id=as.integer(df$user_id)
  df$timestamp=as.POSIXct(df$timestamp/1e3, origin="1970-01-01", tz=tz)
  df
}

#' Find the root_id of a FlyWire segment / supervoxel.
#'
#' @details The main purpose of this function is to convert a supervoxel into
#'   the current root id for the whole segment. If a segment id has been updated
#'   due to editing, calling this the original segment id will still return the
#'   same segment id (although calling it with a supervoxel would return the new
#'   segment id).
#' @param x One or more FlyWire segment ids
#' @param ... Additional arguments passed to \code{\link{pbsapply}} (when more
#'   than 1 id) or to \code{\link{flywire_fetch}}
#'
#' @return A vector of root ids as character vectors
#' @export
#'
#' @examples
#' \dontrun{
#' flywire_rootid("81489548781649724")
#' }
flywire_rootid <- function(x, ...) {
  x=flywire_segments(x, ...)
  stopifnot(all(valid_id(x)))
  if(length(x)>1) {
    res=pbapply::pbsapply(x, flywire_rootid, ...)
    return(res)
  }

  url=sprintf("https://prodv1.flywire-daf.com/segmentation/api/v1/table/fly_v31/node/%s/root?int64_as_str=1", x)
  res=flywire_fetch(url, ...)
  unlist(res, use.names = FALSE)
}


#' Title
#'
#' @param xyz One or more xyz locations as an Nx3 matrix
#' @param rawcoords whether the input values are raw voxel indices or in nm
#' @param voxdims voxel dimensions in nm used to convert the
#' @param cloudvolume.url URL for CloudVolume to fetch segmentation image data.
#'   The default value of NULL choose the production segmentation dataset.
#' @param root Whether to return the root id of the whole segment rather than
#'   the supervoxel id.
#' @param ... additional arguments passed to \code{pbapply} when looking up
#'   multiple positions.
#'
#' @return A character vector of segment ids, \code{NA} when lookup fails.
#' @export
#'
#' @examples
#' \dontrun{
#' # sample dataset from FAFB catmaid
#' n=read.
#' set.seed(42)
#' ss=sample(nvertices(n), size=20)
#' flywire_xyz2id(xyzmatrix(n)[ss,])
#'
#' # here we actually convert to FlyWire coordinate space which should give a
#' # much better match
#' nx=xform_brain(elmr::dense_core_neurons[[1]], ref="FlyWire", sample="FAFB14")
#' flywire_xyz2id(xyzmatrix(nx)[ss,])
#' }
flywire_xyz2id <- function(xyz, rawcoords=FALSE, voxdims=c(4,4,40),
                           cloudvolume.url=NULL,
                           root=TRUE,
                           ...) {
  check_cloudvolume_reticulate()
  if(!is.matrix(xyz)) {
    if(length(xyz)==3) xyz=matrix(xyz, ncol=3)
    else stop("xyz should be an Nx3 matrix!")
  }
  if(isTRUE(rawcoords)) {
    xyz <- scale(xyz, scale = 1/voxdims, center = FALSE)
  }

  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)

  pycode=sprintf(
    "
from cloudvolume import CloudVolume
from cloudvolume import Vec
cv = CloudVolume('%s', use_https=True)

def py_flywire_xyz2id(xyz):
  pt = Vec(*xyz) // cv.meta.resolution(0)
  img = cv.download_point(pt, mip=0, size=1, agglomerate=True)
  return str(img[0,0,0,0])
",cloudvolume.url)

  pydict=py_run_string(pycode)

  safexyz2id <- function(pt) {
    tryCatch(pydict$py_flywire_xyz2id(pt),
             error=function(e) {
               warning(e)
               NA_character_
             })
  }

  res=pbapply::pbapply(xyz, 1, safexyz2id, ...)
  res
}


## Private (for now) helper functions

flywire_cloudvolume_url <- function(cloudvolume.url=NULL, graphene=TRUE) {
  if(is.null(cloudvolume.url))
    cloudvolume.url <- with_segmentation('flywire', getOption("fafbseg.cloudvolume.url"))
  if(isTRUE(graphene)) cloudvolume.url
  else sub("graphene://", "", cloudvolume.url, fixed = T)
}


# ids should be integers >= 0
# this does not check that they are also valid 64 bit ints which might be good
valid_id <- function(x, strict=TRUE) {
  grepl("^[0-9]+$", as.character(x))
}

# private helper function
# TODO merge into ngl_segments
flywire_segments <- function(x, as_character=TRUE, include_hidden=FALSE, cache=TRUE, ...) {
  if(length(x)>1 || is.numeric(x) || valid_id(x)) {
    # assume we have things that look like numbers already
  } else {
    # an URL?
    x=flywire_expandurl(x, cache=cache, ...)
    x=ngl_decode_scene(x)
  }
  ngl_segments(x, as_character=as_character, include_hidden=include_hidden)
}

# private helper function
# FIXME either put this into ngl_decode_scene or decide to export
flywire_expandurl <- function(x, json.only=FALSE, cache=TRUE, ...) {
  pu=try(httr::parse_url(x), silent = TRUE)
  if(!inherits(pu, 'try-error') && !is.null(pu$scheme)) {
    # definitely an URL
    if(length(pu$query)>0) {
      # what we normally get from the link shortener
      x=pu$query$json_url
    }
    x=flywire_fetch(x, cache=cache, return='text', ...)
  }
  if(isFALSE(json.only)) {
    x=with_segmentation('flywire31', ngl_encode_url(x))
  }
  x
}
