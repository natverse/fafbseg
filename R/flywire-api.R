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
#' \donttest{
#' flywire_change_log("720575940619010932")
#' flywire_change_log("720575940619010932", root_ids = TRUE)
#' flywire_change_log("720575940619010932", filtered = FALSE)
#' # with a flywire URL
#' u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5409525645443072"
#' flywire_change_log(u)
#' }
flywire_change_log <- function(x, root_ids=FALSE, filtered=TRUE, tz="UTC", ...) {
  x=ngl_segments(x, as_character = TRUE, include_hidden = FALSE, ...)
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
  df=as.data.frame(lapply(res, unlist), stringsAsFactors=FALSE)
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
#'
#'   There are two \code{method}s. flywire is simpler but will be slower for
#'   many supervoxels since each id requires a separate http request.
#'
#' @param x One or more FlyWire segment ids
#' @param method Whether to use the flywire API (slow but no python required) OR
#'   cloudvolume (faster for many input ids, but requires python). "auto" (the
#'   default) will choose "flywire" for length 1 queries, "cloudvolume"
#'   otherwise.
#' @param cloudvolume.url An optional URL specifying the chunked graph server to
#'   which CloudVolume will connect. When NULL (the default), the
#' @param ... Additional arguments passed to \code{\link{pbsapply}} and
#'   eventually \code{\link{flywire_fetch}} when \code{method="flywire"} OR to
#'   \code{cv$CloudVolume} when \code{method="cloudvolume"}
#'
#' @return A vector of root ids as character vectors named by the input
#'   supervoxel ids.
#' @export
#'
#' @examples
#' \dontrun{
#' flywire_rootid(c("81489548781649724", "80011805220634701"))
#' }
flywire_rootid <- function(x, method=c("auto", "cloudvolume", "flywire"),
                           cloudvolume.url=NULL, ...) {
  method=match.arg(method)
  x=ngl_segments(x, as_character = TRUE, include_hidden = FALSE, ...)
  stopifnot(all(valid_id(x)))

  if(method=="auto" &&  length(x)>1 && requireNamespace('reticulate')
     && reticulate::py_module_available('cloudvolume'))
    method="cloudvolume"
  else method="flywire"

  ids <- if(method=="flywire") {
    if(length(x)>1) {
      pbapply::pbsapply(x, flywire_rootid, method="flywire", ...)
    } else {
      url=sprintf("https://prodv1.flywire-daf.com/segmentation/api/v1/table/fly_v31/node/%s/root?int64_as_str=1", x)
      res=flywire_fetch(url, ...)
      unlist(res, use.names = FALSE)
    }
  } else {
    cv <- check_cloudvolume_reticulate()
    cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)
    vol <- cv$CloudVolume(cloudpath = cloudvolume.url, use_https=TRUE, ...)

    res=reticulate::py_call(vol$get_roots, x)
    pyids2bit64(res)
  }
  if(!isTRUE(length(ids)==length(x)))
    stop("Failed to retrieve root ids for all input ids!")
  names(ids)=x
  ids
}

#' Find all the supervoxel ids that are part of a FlyWire object
#'
#' @param mip The mip level for the segmentation (expert use only)
#' @param bbox The bounding box within which to find supervoxels (default =
#'   \code{NULL} for whole brain. Expert use only.)
#' @param vol A CloudVolume object (expert use only)
#' @export
#' @inheritParams flywire_rootid
#' @seealso \code{\link{flywire_rootid}}
flywire_leaves <- function(x, cloudvolume.url=NULL, mip=0L, bbox=NULL, vol=NULL, ...) {
  x=ngl_segments(x, as_character = TRUE, include_hidden = FALSE, ...)
  stopifnot(all(valid_id(x)))

  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)
  cv <- check_cloudvolume_reticulate()

  if(is.null(vol))
    vol <- cv$CloudVolume(cloudpath = cloudvolume.url, use_https=TRUE, ...)
  if(is.null(bbox)) bbox=vol$meta$bounds(0L)
  if(length(x)>1) {
    res=pbapply::pblapply(x, flywire_leaves, mip=mip, bbox=bbox, vol=vol,
                          cloudvolume.url=cloudvolume.url, ...)
    return(res)
  }

  res=reticulate::py_call(vol$get_leaves, x, mip=mip, bbox=bbox)
  ids=pyids2bit64(res)
  ids
}

#' Title
#'
#' @param xyz One or more xyz locations as an Nx3 matrix or in any form
#'   compatible with \code{\link{xyzmatrix}} including \code{neuron} or
#'   \code{mesh3d} surface objects.
#' @param rawcoords whether the input values are raw voxel indices or in nm
#' @param voxdims voxel dimensions in nm used to convert the
#' @param cloudvolume.url URL for CloudVolume to fetch segmentation image data.
#'   The default value of NULL choose the production segmentation dataset.
#' @param root Whether to return the root id of the whole segment rather than
#'   the supervoxel id.
#' @param ... additional arguments passed to \code{pbapply} when looking up
#'   multiple positions.
#'
#' @details Note that finding the supervoxel for a given XYZ location is order
#'   3x faster than finding the root id for the agglomeration of all of the
#'   super voxels in a given object. Perhaps less intuitively, if you want to
#'   look up many root ids, it is actually quicker to do
#'   \code{flywire_xyz2id(,root=F)} followed by \code{\link{flywire_rootid}}
#'   since that function can look up many root ids in a single call to the
#'   ChunkedGraph server.
#'
#' @return A character vector of segment ids, \code{NA} when lookup fails.
#' @export
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # example based on defining some sample points from real neurons
#' # sample dataset from FAFB catmaid
#' n=elmr::dense_core_neurons[[1]]
#' set.seed(42)
#' ss=sample(nvertices(n), size=10)
#' # first convert FAF14 points to FlyWire coordinate space
#' nx=xform_brain(elmr::dense_core_neurons[[1]], ref="FlyWire", sample="FAFB14")
#' pts=xyzmatrix(nx)[ss,]
#' }
#'
#' # for simplicity just define those same sample points directly
#' pts = matrix(
#' c(428910.52, 110629.64, 174800, 410201.86, 110419.1, 180400,
#'   337136, 129926.28, 189280, 349981.85, 136041.53, 199280, 398361.74,
#'   110731.26, 182640, 382789.26, 118987.04, 189920, 358033.92, 171592.92,
#'   143960, 360990.48, 140277.42, 201400, 376258.06, 125201.51, 194400,
#'   331370.31, 128338.58, 181760),
#' ncol = 3, byrow = TRUE,
#' dimnames = list(NULL, c("X", "Y", "Z"))
#' )
#'
#'
#'
#' # now find the ids for the selected xyz locations
#' flywire_xyz2id(pts)
#'
#' # we can also find the supervoxels - much faster
#' svids=flywire_xyz2id(pts, root=FALSE)
#'
#' # now look up the root ids - very fast with method="cloudvolume", the default
#' flywire_rootid(svids)
#' }
flywire_xyz2id <- function(xyz, rawcoords=FALSE, voxdims=c(4,4,40),
                           cloudvolume.url=NULL,
                           root=TRUE,
                           ...) {
  check_cloudvolume_reticulate()
  if(isTRUE(is.vector(xyz) && length(xyz)==3)) {
    xyz=matrix(xyz, ncol=3)
  } else {
    xyz=xyzmatrix(xyz)
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

def py_flywire_xyz2id(xyz, agglomerate):
  pt = Vec(*xyz) // cv.meta.resolution(0)
  img = cv.download_point(pt, mip=0, size=1, agglomerate=agglomerate)
  return str(img[0,0,0,0])
",cloudvolume.url)

  pydict=reticulate::py_run_string(pycode)

  safexyz2id <- function(pt) {
    tryCatch(pydict$py_flywire_xyz2id(pt, agglomerate=root),
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
