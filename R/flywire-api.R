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
    res=nat::nlapply(x, flywire_change_log, OmitFailures=OmitFailures, tz=tz, ...)
    # otherwise bind_rows has trouble
    class(res)="list"
    df=dplyr::bind_rows(res, .id='id')
    # the rownames are ugly and not useful
    rownames(df)=NULL
    return(df)
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
#'   due to editing, calling with the original segment id will still return the
#'   same segment id (although calling it with a supervoxel would return the new
#'   segment id). If you wish to find the latest root id, then you can use
#'   \code{\link{flywire_latestid}}, but this is fairly slow (think 1 second per
#'   neuron). However in general it is best to select an XYZ location defining a
#'   neuron of interest, or the associated supervoxel id and store that, since
#'   these can be very rapidly looked up by \code{\link{flywire_xyz2id}} and
#'   \code{\link{flywire_rootid}}.
#'
#'   There are two \code{method}s. flywire is simpler but will be slower for
#'   many supervoxels since each id requires a separate http request.
#'
#' @param x One or more FlyWire segment ids
#' @param method Whether to use the flywire API (slow but no python required) OR
#'   cloudvolume (faster for many input ids, but requires python). "auto" (the
#'   default) will choose "flywire" for length 1 queries, "cloudvolume"
#'   otherwise.
#' @param integer64 Whether to return ids as integer64 type (more compact but a
#'   little fragile) rather than character (default \code{FALSE}).
#' @param ... Additional arguments passed to \code{\link{pbsapply}} and
#'   eventually \code{\link{flywire_fetch}} when \code{method="flywire"} OR to
#'   \code{cv$CloudVolume} when \code{method="cloudvolume"}
#'
#' @inheritParams flywire_xyz2id
#'
#' @return A vector of root ids as character vectors.
#'
#' @export
#' @seealso \code{\link{flywire_latestid}}
#' @examples
#' \donttest{
#' flywire_rootid(c("81489548781649724", "80011805220634701"))
#' # same but using the flywire sandbox segmentation
#' with_segmentation('sandbox', {
#' flywire_rootid(c("81489548781649724", "80011805220634701"))
#' })
#' }
flywire_rootid <- function(x, method=c("auto", "cloudvolume", "flywire"),
                           integer64=FALSE,
                           cloudvolume.url=NULL, ...) {
  method=match.arg(method)
  x <- if(bit64::is.integer64(x)) {
    stopifnot(all(valid_id(x)))
    as.character(x)
  } else {
    x <- ngl_segments(x, as_character = TRUE, include_hidden = FALSE, ...)
    stopifnot(all(valid_id(x)))
    x
  }

  orig <- NULL
  zeros <- x=="0" | is.na(x)
  if(sum(zeros)>0) {
    orig <- x
    x <- x[!zeros]
    if(length(x)==0) {
      warning("no valid input ids")
      return(orig)
    }
  }

  if(method=="auto" &&  length(x)>1 && requireNamespace('reticulate')
     && reticulate::py_module_available('cloudvolume'))
    method="cloudvolume"

  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)

  if(any(duplicated(x))) {
    uids=bit64::as.integer64(unique(x))
    unames=flywire_rootid(uids, method=method, integer64=integer64, cloudvolume.url = cloudvolume.url, ...)
    ids <- unames[match(bit64::as.integer64(x), uids)]
  } else {
    ids <- if(method=="flywire") {
      if(length(x)>1) {
        pbapply::pbsapply(x, flywire_rootid, method="flywire", cloudvolume.url=cloudvolume.url, ...)
      } else {
        url=sprintf("https://prodv1.flywire-daf.com/segmentation/api/v1/table/%s/node/%s/root?int64_as_str=1", basename(cloudvolume.url), x)
        res=flywire_fetch(url, ...)
        unlist(res, use.names = FALSE)
      }
    } else {
      vol <- flywire_cloudvolume(cloudvolume.url, ...)
      res=reticulate::py_call(vol$get_roots, x)
      pyids2bit64(res, as_character = !integer64)
    }
    if(!isTRUE(length(ids)==length(x)))
      stop("Failed to retrieve root ids for all input ids!")
  }

  if(integer64) {
    ids=bit64::as.integer64(ids)
    if(isFALSE(is.null(orig))) {
      orig[!zeros]=NA
      orig=bit64::as.integer64(orig)
    }
  } else ids=as.character(ids)

  if(sum(zeros)>0) {
    orig[!zeros]=ids
    orig
  } else ids
}

#' Find all the supervoxel (leaf) ids that are part of a FlyWire object
#'
#' @param integer64 Whether to return ids as integer64 type (more compact but a
#'   little fragile) rather than character (default \code{FALSE}).
#' @param cache Whether to cache the results of flywire_leaves calls. See
#'   details.
#' @param mip The mip level for the segmentation (expert use only)
#' @param bbox The bounding box within which to find supervoxels (default =
#'   \code{NULL} for whole brain. Expert use only.)
#' @export
#' @inheritParams flywire_rootid
#'
#' @details The caching scheme depends on a least recently used cache (LRU) and
#'   will store up to 5000 results of \code{flywire_leaves} calls. Since each
#'   root id can map to hundreds of thousands of supervoxel ids, there are
#'   memory implications. In order to save space, the results are stored as
#'   compressed 64 bit integers (which are ~30x smaller than character vectors).
#'   The compression step does add an extra ~ 5% time on a cache miss but is
#'   100x + faster on a cache hit.
#' @seealso \code{\link{flywire_rootid}}
flywire_leaves <- function(x, cloudvolume.url=NULL, integer64=FALSE,
                           mip=0L, bbox=NULL, cache=TRUE, ...) {
  x=ngl_segments(x, as_character = TRUE, include_hidden = FALSE, ...)
  stopifnot(all(valid_id(x)))
  # really needs to be an integer
  mip=checkmate::asInteger(mip)

  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)
  if(isTRUE(cache)) {
    if(!is.null(bbox))
      stop("Cannot currently use cache=TRUE with non-standard bounding box")
    compression=if(requireNamespace('brotli', quietly = T)) 'brotli' else 'gzip'
  }

  vol <- flywire_cloudvolume(cloudpath = cloudvolume.url, ...)
  if(isTRUE(cache)) {
    if(length(x)>1) {
      res=pbapply::pbsapply(x, flywire_leaves_cached, integer64=integer64, mip=mip, bbox=bbox,
                            cloudvolume.url=cloudvolume.url, compression=compression, ..., simplify = FALSE)
      return(res)
    } else {
      flywire_leaves_cached(x, integer64=integer64, mip=mip, bbox=bbox,
                            cloudvolume.url=cloudvolume.url, compression=compression,...)
    }
  } else {
    if(length(x)>1) {
      res=pbapply::pbsapply(x, flywire_leaves_impl, integer64=integer64, mip=mip, bbox=bbox,
                            cloudvolume.url=cloudvolume.url, ..., simplify = FALSE)
      return(res)
    } else {
      flywire_leaves_impl(x, integer64=integer64, mip=mip, bbox=bbox,
                            cloudvolume.url=cloudvolume.url,...)
    }
  }
}

flywire_leaves_cached <-
  function(x,
           cloudvolume.url,
           mip,
           bbox,
           integer64,
           ...,
           compression = 'gzip') {
    x = ngl_segments(x, as_character = T)
    compbytes = flywire_leaves_tobytes_memo(
      x,
      mip = mip,
      cloudvolume.url = cloudvolume.url,
      ...,
      type = compression
    )
    bytes = flywire_leaves_frombytes(compbytes, type = compression)
    ids = readBin(bytes, what = double(), n = length(bytes) / 8)
    class(ids) = 'integer64'
    if (integer64)
      ids
    else
      bit64::as.character.integer64(ids)
  }

# private function that does the most basic supervoxel query via CloudVolume
flywire_leaves_impl <- function(x, cloudvolume.url, mip, bbox=NULL, integer64=TRUE, ...) {
  vol <- flywire_cloudvolume(cloudpath = cloudvolume.url, ...)
  if(is.null(bbox)) bbox=vol$meta$bounds(mip)
  res=reticulate::py_call(vol$get_leaves, x, mip=mip, bbox=bbox)
  ids=pyids2bit64(res, as_character=isFALSE(integer64))
  ids
}

# private function that converts flywire_leaves results into
# a maximally efficient compressed representation
flywire_leaves_tobytes <- function(x, cloudvolume.url, mip, ...,
           type = c("gzip", "bzip2", 'xz', 'none', 'snappy', "brotli")) {
    type=match.arg(type)
    ids=flywire_leaves_impl(x, integer64=TRUE, mip=mip,
                            cloudvolume.url=cloudvolume.url, ...)
    bytes=writeBin(unclass(ids), raw())
    if(type=='none') return(bytes)
    if(type=='snappy') stop("not implemented") # snappier::compress_raw(bytes)
    # quality = 2 is actually faster and better than gzip
    if(type=='brotli') brotli::brotli_compress(bytes, quality = 2)
    else memCompress(bytes, type=type)
}
# memoised version of above
flywire_leaves_tobytes_memo <- memo::memo(flywire_leaves_tobytes)

# private: status of cache
#' @importFrom utils object.size
flywire_leaves_cache_stats <- function() {
  m=memo::cache_stats(flywire_leaves_tobytes_memo)
  lru=environment(environment(flywire_leaves_tobytes_memo)$cache)$lru
  sizes=sapply(ls(lru), function(x) object.size(get(x, envir = lru)), USE.NAMES = F)

  c(m, list(sizes=sizes, total=ifelse(length(sizes), sum(sizes), 0)))
}


# (non-memoised) function to decompress the results of above
flywire_leaves_frombytes <- function(x, type=c("gzip", "bzip2", 'xz', 'none', 'snappy', 'brotli')) {
  type=match.arg(type)
  if(type=='none') return(x)
  if(type=='snappy') stop("not implemented") # snappier::decompress_raw(x)
  if(type=='brotli') brotli::brotli_decompress(x)
  else memDecompress(x, type=type)
}

#' Find the most up to date FlyWire rootid for a given input rootid
#'
#' @description Finds the supervoxel ids for the input rootid and then maps
#'   those to their current rootid by simple majority vote.
#' @details By default a sample of the input rootids is used since that step is
#'   the most time consuming part. The sample can be defined as a fraction
#'   (0<sample<1) or an absolute number. They will be clamped to the actual
#'   number of supervoxels in the object.
#'
#'   Note that \code{flywire_latestid} is slow (order 1 second per object). If
#'   you need to do this regularly for a set of neurons is \bold{much} better to
#'   keep an XYZ location or even better a supervoxel id at a safe location on
#'   the neuron such as the primary branch point (typically where the cell body
#'   fibre joins the rest of the neuron).
#' @param rootid A FlyWire rootid defining a segment
#' @param sample An absolute or fractional number of supervoxel ids to map to
#'   rootids or \code{FALSE} (see details).
#' @param Verbose When set to \code{TRUE} prints information about what fraction
#'   of
#' @param ... Additional arguments passed to \code{\link{flywire_leaves}}
#' @inheritParams flywire_rootid
#'
#' @return A character vector of rootids
#' @export
#' @seealso \code{\link{flywire_rootid}}, \code{\link{flywire_xyz2id}},
#'   \code{\link{flywire_leaves}}
#' @examples
#' \donttest{
#'
#' # one of the neurons displayed in the sandbox
#' with_segmentation('sandbox', flywire_latestid('720575940610453042'))
#' \dontrun{
#' #' with_segmentation('sandbox', flywire_latestid('720575940610453042', Verbose = T))
#'
#' # check every supervoxel (slow for bigger neurons, but this KC is smallish)
#' flywire_latestid('720575940616243077', sample=FALSE)
#'
#' # update a neuroglancer URL with the most up to date segment ids
#' # nb this is slow since it looks at all supervoxels - much more efficient to
#' # store a single xyz location or supervoxel id
#' u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5695907068641280"
#' ngl_segments(u) <- pbapply::pbsapply(ngl_segments(u), flywire_latestid)
#' # open modified URL in your browser
#' browseURL(u)
#'
#' }
#' }
flywire_latestid <- function(rootid, sample=1000L, cloudvolume.url=NULL, Verbose=FALSE, ...) {
  svids=flywire_leaves(rootid, cloudvolume.url = cloudvolume.url, integer64 = T, ...)

  if(isTRUE(sample<1)){
    checkmate::check_numeric(sample, lower = 0, upper = 1)
    sample=round(sample*length(svids))
    if(sample<1) sample=1L
  }

  if(!isFALSE(sample)) {
    checkmate::check_integerish(sample, lower = 1, upper = Inf)
    if(sample<length(rootid)) {
      svids=sample(svids, size = sample)
    }
  }

  rootids.new=flywire_rootid(svids, cloudvolume.url = cloudvolume.url, ...)
  tt=table(rootids.new)/length(rootids.new)
  tt=tt[setdiff(names(tt), "0")]
  if(max(tt)<0.5)
    warning("large changes in supervoxel composition for: ", rootid)

  if(Verbose) {
    pct_correct=max(tt)/sum(tt)*100
    message(floor(pct_correct), "% of supervoxel ids match the new rootid!")
  }

  newseg=names(which.max(tt))
  newseg
}


#' Find FlyWire root or supervoxel (leaf) ids for XYZ locations
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
#' @importFrom nat pointsinside
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
#' flywire_xyz2id(pts, fast_root=FALSE, method="cloudvolume")
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
  if(isFALSE(rawcoords) && sum(res==0)>0.25*length(res)) {
    # we got some failures to map, let's see if there was a mistake with
    # what kind of coords we were passed.
    # dput(boundingbox(elmr::FAFB14)/c(4,4,40))
    rawbb=makeboundingbox(c(0, 253951, 0, 155647, 0, 7062))
    if(all(pointsinside(xyz, rawbb))) {
      warning("It looks like you may be passing in raw coordinates. If so, use rawcoords=TRUE")
    }
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
#' @export
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
  if(is.integer64(x) || is.integer(x))
    return(!is.na(x) & x>=0)
  if(is.numeric(x)) {
    return(checkmate::test_double(x, lower=0, upper=(2^53-1), any.missing = F))
  }
  grepl("^\\d{1,19}$", as.character(x), perl = TRUE, useBytes = TRUE)
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
