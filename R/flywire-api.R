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
#' @param OmitFailures Whether to omit neurons for which there is an API timeout
#'   or error. The default value (\code{TRUE}) will skip over errors, while
#'   \code{NA}) will result in a hard stop on error. See \code{\link{nlapply}}
#'   for more details.
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
#'
#' @importFrom nat nlapply progress_natprogress
flywire_change_log <- function(x, root_ids=FALSE, filtered=TRUE, tz="UTC",
                               OmitFailures=TRUE, ...) {
  x=ngl_segments(x, as_character = TRUE, include_hidden = FALSE, ...)
  if(length(x)>1) {
    ## use nlapply for fault tolerance + progress bar
    # need to name input vector to ensure that .id works in bind_rows
    names(x)=x
    res=nat::nlapply(x, flywire_change_log, OmitFailures=OmitFailures, ...)
    # otherwise bind_rows has trouble
    class(res)="list"
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
#' @param ... Additional arguments passed to \code{\link{pbsapply}} and
#'   eventually \code{\link{flywire_fetch}} when \code{method="flywire"} OR to
#'   \code{cv$CloudVolume} when \code{method="cloudvolume"}
#'
#' @inheritParams flywire_xyz2id
#'
#' @return A vector of root ids as character vectors.
#'
#' @export
#'
#' @examples
#' \donttest{
#' flywire_rootid(c("81489548781649724", "80011805220634701"))
#' # same but using the flywire sandbox segmentation
#' with_segmentation('sandbox', {
#' flywire_rootid(c("81489548781649724", "80011805220634701"))
#' })
#' }
flywire_rootid <- function(x, method=c("auto", "cloudvolume", "flywire"),
                           cloudvolume.url=NULL, ...) {
  method=match.arg(method)
  x=ngl_segments(x, as_character = TRUE, include_hidden = FALSE, ...)
  stopifnot(all(valid_id(x)))

  orig <- NULL
  zeros <- x=="0"
  if(sum(zeros)>0) {
    orig <- x
    x <- x[!zeros]
  }

  if(method=="auto" &&  length(x)>1 && requireNamespace('reticulate')
     && reticulate::py_module_available('cloudvolume'))
    method="cloudvolume"
  else method="flywire"

  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)
  ids <- if(method=="flywire") {
    if(length(x)>1) {
      pbapply::pbsapply(x, flywire_rootid, method="flywire", ...)
    } else {
      url=sprintf("https://prodv1.flywire-daf.com/segmentation/api/v1/table/%s/node/%s/root?int64_as_str=1", basename(cloudvolume.url), x)
      res=flywire_fetch(url, ...)
      unlist(res, use.names = FALSE)
    }
  } else {
    vol <- flywire_cloudvolume(cloudvolume.url, ...)
    res=reticulate::py_call(vol$get_roots, x)
    pyids2bit64(res)
  }
  if(!isTRUE(length(ids)==length(x)))
    stop("Failed to retrieve root ids for all input ids!")

  if(sum(zeros)>0) {
    orig[!zeros]=ids
    orig
  } else ids
}

#' Find all the supervoxel ids that are part of a FlyWire object
#'
#' @param mip The mip level for the segmentation (expert use only)
#' @param bbox The bounding box within which to find supervoxels (default =
#'   \code{NULL} for whole brain. Expert use only.)
#' @export
#' @inheritParams flywire_rootid
#' @seealso \code{\link{flywire_rootid}}
flywire_leaves <- function(x, cloudvolume.url=NULL, mip=0L, bbox=NULL, ...) {
  x=ngl_segments(x, as_character = TRUE, include_hidden = FALSE, ...)
  stopifnot(all(valid_id(x)))
  # really needs to be an integer
  mip=checkmate::asInteger(mip)

  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)

  vol <- flywire_cloudvolume(cloudpath = cloudvolume.url, ...)
  if(is.null(bbox)) bbox=vol$meta$bounds(mip)
  if(length(x)>1) {
    res=pbapply::pblapply(x, flywire_leaves, mip=mip, bbox=bbox, vol=vol,
                          cloudvolume.url=cloudvolume.url, ...)
    return(res)
  }

  res=reticulate::py_call(vol$get_leaves, x, mip=mip, bbox=bbox)
  ids=pyids2bit64(res)
  ids
}

#' Find FlyWire root or supervoxel ids for XYZ locations
#'
#' @param xyz One or more xyz locations as an Nx3 matrix or in any form
#'   compatible with \code{\link{xyzmatrix}} including \code{neuron} or
#'   \code{mesh3d} surface objects.
#' @param rawcoords whether the input values are raw voxel indices or in nm
#' @param voxdims voxel dimensions in nm used to convert the
#' @param cloudvolume.url URL for CloudVolume to fetch segmentation image data.
#'   The default value of NULL chooses the flywire production segmentation
#'   dataset.
#' @param root Whether to return the root id of the whole segment rather than
#'   the supervoxel id.
#' @param method Whether to use the
#'   \href{https://spine.janelia.org/app/transform-service/docs}{spine
#'   transform-service} API or cloudvolume for lookup. \code{"auto"} is
#'   presently a synonym for \code{"spine"}.
#' @param fast_root Whether to use a fast but two-step look-up procedure when
#'   finding roots. This is strongly recommended and the alternative approach
#'   has only been retained for validation purposes.
#' @param ... additional arguments passed to \code{pbapply} when looking up
#'   multiple positions.
#'
#' @details root ids define a whole neuron or segmented object. supervoxel ids
#'   correspond to a small group of voxels that it is assumed must all belong to
#'   the same object. supervoxel ids do not change for a given a segmentation,
#'   whereas root ids change every time a neuron is edited. The most stable way
#'   to refer to a FlyWire neuron is to choose a nice safe location on the
#'   arbour (I recommend a major branch point) and then store the supervoxel id.
#'   You can rapidly map the supervoxel id to the current root id using
#'   \code{\link{flywire_rootid}}.
#'
#'   As of November 2020, the default approach to look up supervoxel ids for a
#'   3D point is using the
#'   \href{https://spine.janelia.org/app/transform-service/docs}{spine
#'   transform-service} API. This is order 100x faster than mapping via
#'   cloudvolume (since that must make a single web request for every point) and
#'   Eric Perlman has optimised the layout of the underlying data for rapid
#'   mapping.
#'
#'   Note that finding the supervoxel for a given XYZ location is order 3x
#'   faster than finding the root id for the agglomeration of all of the super
#'   voxels in a given object. Perhaps less intuitively, if you want to look up
#'   many root ids, it is actually quicker to do \code{flywire_xyz2id(,root=F)}
#'   followed by \code{\link{flywire_rootid}} since that function can look up
#'   many root ids in a single call to the ChunkedGraph server. We now offer the
#'   option to do this for the user when setting \code{fast_root=TRUE}.
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
#' #' # Fast and simple appoach to find ids in one (user-facing) step
#' flywire_xyz2id(pts)
#'
#'
#' ## illustrate what's happening under the hood
#'
#' # find the ids for the selected xyz locations
#' # NB fast_root=FALSE was the default behaviour until Nov 2020
#' flywire_xyz2id(pts, fast_root=FALSE)
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
                           fast_root=TRUE,
                           method=c("auto", "cloudvolume", "spine"),
                           ...) {
  check_cloudvolume_reticulate()
  method=match.arg(method)
  if(isTRUE(is.vector(xyz) && length(xyz)==3)) {
    xyz=matrix(xyz, ncol=3)
  } else {
    xyz=xyzmatrix(xyz)
  }
  if(isTRUE(rawcoords)) {
    xyz <- scale(xyz, scale = 1/voxdims, center = FALSE)
  } else {
  }

  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)

  if(method %in% c("auto", "spine")) {
    if(isTRUE(root) && isFALSE(fast_root)) {
      warning("You must use fast_root=TRUE when mapping with method=",method)
      fast_root=TRUE
    }
    res=flywire_supervoxels(xyz)
  } else {
    cv=flywire_cloudvolume(cloudvolume.url = cloudvolume.url)
    pycode=sprintf(
      "
from cloudvolume import Vec

def py_flywire_xyz2id(cv, xyz, agglomerate):
  pt = Vec(*xyz) // cv.meta.resolution(0)
  img = cv.download_point(pt, mip=0, size=1, agglomerate=agglomerate)
  return str(img[0,0,0,0])
")

    pydict=reticulate::py_run_string(pycode)

    safexyz2id <- function(pt) {
      tryCatch(pydict$py_flywire_xyz2id(cv, pt, agglomerate=root && !fast_root),
               error=function(e) {
                 warning(e)
                 NA_character_
               })
    }

    res=pbapply::pbapply(xyz, 1, safexyz2id, ...)
  }

  if(fast_root && root) {
    res=flywire_rootid(res, cloudvolume.url = cloudvolume.url)
  }
  res
}

#' Low level access to FlyWire data via Python cloudvolume module
#'
#' @details this is the equivalent of doing (in Python):
#'
#'   \verb{from cloudvolume import CloudVolume vol =
#'   CloudVolume('graphene://https://prodv1.flywire-daf.com/segmentation/table/fly_v31',
#'   use_https=True)}
#'
#'   The cache tries to be intelligent by \itemize{
#'
#'   \item 1. generating a new object for every input parameter combination
#'   (which of course you would need to do in Python)
#'
#'   \item 2. avoiding stale references by checking that Python is currently
#'   running and that the returned CloudVolume object is non-null. It also
#'   regenerates the object every hour.}
#'
#'   Note that reticulate the package which allows R/Python interaction binds to
#'   one Python session. Furthermore Python cannot be restarted without also
#'   restarting R.
#' @param cached When \code{TRUE} (the default) reuses a cached CloudVolume
#'   object from the current Python session. See details.
#' @param ... Additional arguments  passed to the CloudVolume constructor
#' @inheritParams flywire_xyz2id
#' @importFrom memoise forget memoise timeout
#'
#' @examples
#' \dontrun{
#' cv=flywire_cloudvolume()
#'
#' # detailed info about the image volume
#' cv$info
#' # bounding box (Python format in raw voxels)
#' cv$bounds
#' # in nm
#' boundingbox(cv)
#'
#' # get help for a function
#' reticulate::py_help(cv$get_roots)
#' }
flywire_cloudvolume <- function(cloudvolume.url=NULL, cached=TRUE, ...) {
  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)
  if(!isTRUE(cached) || !reticulate::py_available())
    forget(flywire_cloudvolume_memo)
  vol <- flywire_cloudvolume_memo(cloudvolume.url, ...)
  # just in case we end up with a stale reference from a previous python session
  if(reticulate::py_is_null_xptr(vol)) {
    forget(flywire_cloudvolume_memo)
    vol <- flywire_cloudvolume_memo(cloudvolume.url, ...)
  }
  vol
}

flywire_cloudvolume_memo <- memoise( function(cloudvolume.url, ...) {
  cv <- check_cloudvolume_reticulate()
  vol <- cv$CloudVolume(cloudpath = cloudvolume.url, use_https=TRUE, ...)
  vol
}, ~timeout(3600))


## Private (for now) helper functions

flywire_cloudvolume_url <- function(cloudvolume.url=NULL, graphene=TRUE) {
  if(is.null(cloudvolume.url)) {
    u=getOption("fafbseg.cloudvolume.url")
    # the current option points to a flywire URL so use that
    cloudvolume.url <- if(grepl("flywire", u, fixed = TRUE))
      u else
      with_segmentation('flywire', getOption("fafbseg.cloudvolume.url"))
  }
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

# need to harmonise URLs etc, but this is a big speed-up, so should just get on
# with rolling it out
flywire_supervoxels <- function(x, voxdims=c(4,4,40)) {
  pts=scale(xyzmatrix(x), center = F, scale = voxdims)
  nas=rowSums(is.na(pts))>0
  if(any(nas)) {
    svids=rep("0", nrow(pts))
    svids[!nas]=flywire_supervoxels(pts[!nas,,drop=F], voxdims = c(1,1,1))
    return(svids)
  }

  u="https://spine.janelia.org/app/transform-service/query/dataset/flywire_190410/s/2/values_array_string_response"
  body=jsonlite::toJSON(list(x=pts[,1], y=pts[,2], z=pts[,3]))
  res=httr::POST(u, body = body)
  httr::stop_for_status(res)
  j=httr::content(res, as='text', encoding = 'UTF-8')
  svids=unlist(jsonlite::fromJSON(j, simplifyVector = T), use.names = F)

}

flywire_supervoxels_binary <- function(x, voxdims=c(4,4,40)) {
  pts=scale(xyzmatrix(x), center = F, scale = voxdims)
  ptsb=writeBin(as.vector(pts), con = raw(), size=4)
  u="https://spine.janelia.org/app/transform-service/query/dataset/flywire_190410/s/2/values_binary/format/array_float_3xN"

  res=httr::POST(u, body=ptsb, encode = "raw")
  httr::stop_for_status(res)
  arr=httr::content(res)
  bytes=readBin(arr, what = numeric(), n=length(arr)/8, size = 8, endian = 'little')
  class(bytes)="integer64"
  bit64::as.character.integer64(bytes)
}
