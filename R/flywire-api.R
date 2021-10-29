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
#'   Default \code{TRUE}. As of June 2021 root_ids are always returned, so this
#'   is a noop.
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
#'   \item{before_root_ids and after_root_ids}{ as space separated strings}
#'
#'   }
#'
#'   In addition when \code{filtered=FALSE}, \code{in_neuron} \code{is_relevant}
#'
#' @export
#'
#' @examples
#' \donttest{
#' flywire_change_log("720575940619010932")
#' flywire_change_log("720575940619010932", filtered = FALSE)
#' # with a flywire URL
#' u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5409525645443072"
#' flywire_change_log(u)
#' }
#'
#' @importFrom nat nlapply progress_natprogress
flywire_change_log <- function(x, filtered=TRUE, tz="UTC",
                               root_ids=TRUE, OmitFailures=TRUE, ...) {
  x=ngl_segments(x, as_character = TRUE, include_hidden = FALSE, ...)
  if(isFALSE(root_ids))
    warning("root_ids=FALSE is no longer supported")
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

  pu=httr::parse_url(flywire_api_url(endpoint = "root/%s/tabular_change_log"))

  pu$path=sprintf(pu$path, x)
  pu$query=list(root_ids=as.character(root_ids),
                filtered=as.character(filtered))
  url=httr::build_url(pu)
  res=flywire_fetch(url, ...)
  if(!is.null(res[['before_root_ids']])) {
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
#'   There are two \code{method}s. flywire is simpler but will be much slower
#'   for many supervoxels since each id requires a separate http request.
#'
#'   The cloudvolume method is \emph{much} faster and can process hundreds of
#'   ids per second. In order to avoid sending too many requests in one go
#'   (there is a limit to the post message size), id lookups are chunked into a
#'   maximum of 100,000 lookups in a single call. This can be modified by
#'   passing in the \code{chunksize} argument or setting the
#'   \code{fafbseg.flywire_roots.chunksize} option. Set this smaller if the
#'   queries time out / give "Request Entity Too Large" errors. Larger settings
#'   are unlikely to have much of a speed impact.
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
#' @family flywire-ids
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
      flywire_roots_cv(x, cloudvolume.url=cloudvolume.url, integer64 = integer64, ...)
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

# private function to process roots, but ensuring that we don't exceed a maximum
# chunk size
flywire_roots_cv <- function(x, cloudvolume.url,
                             chunksize=getOption('fafbseg.flywire_roots.chunksize',1e5),
                              ..., integer64=TRUE) {
  nx=length(x)
  nchunks=ceiling(nx/chunksize)
  checkmate::assert_int(nchunks, lower=1)
  chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nx)]
  chunkstoread=seq_len(nchunks)
  vol <- flywire_cloudvolume(cloudvolume.url, ...)

  if(interactive())
    pb <- progress::progress_bar$new(total = nx, show_after=2,
                                   format = "  flywire_roots [:bar] :percent eta: :eta")

  res=list()
  for(i in chunkstoread) {
    pyres=reticulate::py_call(vol$get_roots, x[chunks==i])
    res[[length(res)+1]]=pyids2bit64(pyres, as_character = !integer64)
    if(interactive())
      pb$tick(length(res[[length(res)]]))
  }
  vres=unlist(res, use.names = F)
  if(integer64)
    class(vres)='integer64'
  vres
}


#' Find all the supervoxel (leaf) ids that are part of a FlyWire object
#'
#' @description This workhorse function underlies the ability to define synaptic
#'   connections and neurotransmitter predictions for flywire neurons.
#'
#' @param integer64 Whether to return ids as integer64 type (the default, more
#'   compact but a little fragile) rather than character (when \code{FALSE}).
#' @param cache Whether to cache the results of flywire_leaves calls. See
#'   details.
#' @param mip The mip level for the segmentation (expert use only)
#' @param bbox The bounding box within which to find supervoxels (default =
#'   \code{NULL} for whole brain. Expert use only.)
#' @export
#' @inheritParams flywire_rootid
#'
#' @details By default repeated calls to \code{flywire_leaves} are cached on
#'   disk. This functionality is provided by the \code{cachem} package now used
#'   by the \code{memoise} package. By default the cache will expand up to 1.5GB
#'   and then start pruning on a least recently used basis (LRU). 1.5GB might
#'   store 10-20,000 results for \code{flywire_leaves} calls depending on the
#'   size of the corresponding neurons.
#'
#'   Since each root id can map to hundreds of thousands of supervoxel ids,
#'   there are space implications. In order to save space, the results are
#'   stored as compressed 64 bit integers (which are ~30x smaller than character
#'   vectors). The compression step does add an extra ~ 5% time on a cache miss
#'   but is 100x + faster on a cache hit. The default compression is based on
#'   the suggested brotli library if available, gzip otherwise.
#'
#'   There is functionality for a memory cache on top of the disk cache, but
#'   this is not currently exposed as the disk read time appears small compared
#'   with the time for uncompressing and other overheads.
#'
#'   The cache can be controlled by two package options:
#'
#'   \itemize{
#'
#'   \item \code{fafbseg.cachedir} The location on disk. If not previously set,
#'   it is set to an appropriate user folder on package load using
#'   \code{rappdirs::\link[rappdirs]{user_data_dir}}. Note that the cache for
#'   this function will be located inside a folder called \code{flywire_leaves}.
#'
#'   \item \code{fafbseg.flcachesize} The maximum cache size in bytes. When the
#'   storage space exceeds this results are pruned using a LRU algorithm.
#'   Defaults to \code{1.5 * 1024^3} when unset.
#'
#'   }
#'
#'   Note that the default configuration means that the cache will be shared for
#'   a given user across R sessions. It is worth bearing in mind the possibility
#'   of race conditions if multiple applications are writing/pruning the cache.
#'   For example if the \code{fafbseg.flcachesize} has different values in
#'   different sessions, the session with the smallest value will start pruning
#'   files on disk before the other session.
#' @family flywire-ids
#'
#' @examples
#' \donttest{
#' kcid="720575940623755722"
#' length(flywire_leaves(kcid))
#' }
#' \dontrun{
#' # developer function to check cache status
#' fafbseg:::flywire_leaves_cache_info()
#' }
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
    cache=flywire_leaves_cache()
    # nb hash the cloudvolume URL since key is only lower case alphanumeric
    key=paste0(x, sep="ooo", digest::digest(cloudvolume.url, algo = 'xxhash64'))
    value=cache$get(key)
    if(cachem::is.key_missing(value)) {
      # not in the cache, will look up remotely and convert to compressed bytes
      compbytes=flywire_leaves_tobytes(
        x,
        mip = mip,
        cloudvolume.url = cloudvolume.url,
        ...,
        type = compression
      )
      cache$set(key, compbytes)
    } else {
      compbytes = value
    }
    # now we need to turn compressed bytes back into ids
    bytes=flywire_leaves_frombytes(compbytes, type = compression)
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

# define a cachem cache to hold results
# (non-memoised) function to decompress the results of above
flywire_leaves_frombytes <- function(x, type=c("gzip", "bzip2", 'xz', 'none', 'snappy', 'brotli')) {
  type=match.arg(type)
  if(type=='none') return(x)
  if(type=='snappy') stop("not implemented") # snappier::decompress_raw(x)
  if(type=='brotli') {
    tryCatch(brotli::brotli_decompress(x), error=function(e) {
      # we have a mix of brotli and gzip in many cases
      memDecompress(x, type='gzip')
    })
  }
  else memDecompress(x, type=type)
}

# memoised so that we can change cache dir during a session but not make more
# than one cache object per condition
flywire_leaves_cache <- memoise::memoise(function(
  cachedir=getOption("fafbseg.cachedir"),
  cachesize=getOption("fafbseg.flcachesize", 1.5 * 1024^3),
  hybrid=FALSE) {
  check_package_available('cachem')
  # so we can use cachedir for other caches.
  if(isTRUE(nzchar(cachedir))) cachedir=file.path(cachedir, "flywire_leaves")
  d <- cachem::cache_disk(max_size = cachesize, dir = cachedir)
  if(isTRUE(hybrid)) {
    # unclear that mem cache gives any useful benefit given compression cycle
    m <- cachem::cache_mem(max_size = 200 * 1024^2)
    cl <- cachem::cache_layered(m, d)
    cl
  } else d
})

# private: status of cache
flywire_leaves_cache_info <- function() {
  cache <- flywire_leaves_cache()
  ci <- cache$info()
  ff=dir(ci$dir, full.names = TRUE)
  c(ci, nitems=cache$size(), current_size=sum(file.size(ff)))
}

#' Find the most up to date FlyWire rootid for one or more input rootids
#'
#' @description Finds the supervoxel ids for the input rootid and then maps
#'   those to their current rootid by simple majority vote.
#' @details By default a sample of the input rootids is used since that step is
#'   the most time consuming part. The sample can be defined as a fraction
#'   (0<sample<1) or an absolute number. They will be clamped to the actual
#'   number of supervoxels in the object.
#'
#'   \code{flywire_latestid} does a precheck to see if the input rootids have
#'   been updated using \code{\link{flywire_islatest}}; this precheck is very
#'   fast (thousands of neurons per second). Only those ids that are not to date
#'   are then further processed to identify the new rootid. This second step is
#'   slow (order 1-10 s per object). If you need to do this regularly for a set
#'   of neurons, it is \bold{much} better to keep an XYZ location or even better
#'   a supervoxel id at a safe location on the neuron such as the primary branch
#'   point (typically where the cell body fibre joins the rest of the neuron).
#'
#'   Note that after edits that remove pieces of a starting neuron,
#'   flywire_latestid will return the id of the largest resultant piece.
#'
#' @param rootid One ore more FlyWire rootids defining a segment (in any form
#'   interpretable by \code{\link{ngl_segments}})
#' @param sample An absolute or fractional number of supervoxel ids to map to
#'   rootids or \code{FALSE} (see details).
#' @param method \code{"cave"} uses the \code{caveclient} python module, which
#'   is generally faster, but has the disadvantage that it does not disambiguate
#'   between the two options after a split. "auto" chooses cave when available
#'   "leaves" otherwise.
#' @param Verbose When set to \code{TRUE} prints information about what fraction
#'   of
#' @param ... Additional arguments passed to \code{\link{flywire_leaves}}
#' @inheritParams flywire_rootid
#'
#' @return A character vector of rootids. When the input is 0 or NA, the output
#'   will be 0.
#' @export
#' @family flywire-ids
#' @examples
#' \donttest{
#'
#' # one of the neurons displayed in the sandbox
#' with_segmentation('sandbox', flywire_latestid('720575940625602908'))
#' \dontrun{
#' with_segmentation('sandbox', flywire_latestid('720575940625602908', Verbose = T))
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
flywire_latestid <- function(rootid, sample=1000L, cloudvolume.url=NULL,
                             Verbose=FALSE, method=c("auto", "leaves", "cave"), ...) {
  if(Verbose) message("Checking if any ids are out of date")

  ids=ngl_segments(rootid, as_character = TRUE, must_work = FALSE)
  fil=flywire_islatest(ids)
  needsupdate=!fil & !is.na(fil)
  method=match.arg(method)
  if(method=='auto') {
    cave_avail=!inherits(try(check_cave(), silent = T), 'try-error')
    method=ifelse(cave_avail, "cave", "leaves")
  }
  if(any(needsupdate)) {
    if(Verbose) message("Looking up ", sum(needsupdate), " outdated ids!")
    new=pbapply::pbsapply(ids[needsupdate], .flywire_latestid,
                          cloudvolume.url=cloudvolume.url,
                          sample=sample, Verbose=Verbose,
                          method=method, ...)
    ids[needsupdate]=new
    return(ids)
  } else return(ids)
}
# private function
.flywire_latestid <- function(rootid, cloudvolume.url, method, ..., sample, Verbose) {
  svids=flywire_leaves(rootid, cloudvolume.url = cloudvolume.url, integer64 = T, ...)
  if(method=='cave') {
    newseg=cave_latestid(rootid, ..., integer64 = FALSE)
    if(length(newseg)==0) newseg=NA
    if(length(newseg)==1) return(newseg)
    warning('Ambiguous results for ', rootid,
            ' with method="cave", trying method="leaves"')
  }
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
#' @param voxdims voxel dimensions in nm used to convert raw coordinates.
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
#' @family flywire-ids
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
  if(isTRUE(is.numeric(xyz) && is.vector(xyz) && length(xyz)==3)) {
    xyz=matrix(xyz, ncol=3)
  } else {
    xyz=xyzmatrix(xyz)
  }
  if(isTRUE(rawcoords)) {
    xyz <- scale(xyz, scale = 1/voxdims, center = FALSE)
  }
  checkmate::assertNumeric(xyz)

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
#'    use_https=True)}
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
#' @param min_version A minimum version for the cloudvolume Python module e.g.
#'   \code{"3.12"}. The default \code{NULL} implies any version is acceptable.
#' @param ... Additional arguments  passed to the CloudVolume constructor
#' @inheritParams flywire_xyz2id
#' @importFrom memoise forget memoise timeout
#' @seealso \code{\link{simple_python}} for installation of the necessary Python
#'   packages.
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
flywire_cloudvolume <- function(cloudvolume.url=NULL, cached=TRUE,
                                min_version=NULL, ...) {
  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = TRUE)
  if(!isTRUE(cached) || !reticulate::py_available())
    forget(flywire_cloudvolume_memo)
  vol <- flywire_cloudvolume_memo(cloudvolume.url, min_version=min_version, ...)
  # just in case we end up with a stale reference from a previous python session
  if(reticulate::py_is_null_xptr(vol)) {
    forget(flywire_cloudvolume_memo)
    vol <- flywire_cloudvolume_memo(cloudvolume.url, min_version=min_version, ...)
  }
  vol
}

flywire_cloudvolume_memo <- memoise( function(cloudvolume.url,
                                              min_version=NULL, ...) {
  cv <- check_cloudvolume_reticulate(min_version=min_version)
  vol <- cv$CloudVolume(cloudpath = cloudvolume.url, use_https=TRUE, ...)
  vol
}, ~timeout(3600))


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


#' Check that one or more FlyWire root ids have not been further edited
#'
#' @details This call is quite fast (think thousands of ids per second). The
#'   current implementation also de-duplicates the input automatically. You can
#'   pass in a vector containing duplicates and only the unique ids will be
#'   passed on to the server.
#'
#'   If you provide input as \code{integer64} then data will be sent in binary
#'   form to the flywire server. This can have a significant time saving for
#'   large queries (think 10000+).
#'
#'   When a \code{timestamp} is provided, only edits up until that time point
#'   will be considered. Note that \code{flywire_islatest} will return
#'   \code{TRUE} in the case of a rootid that was not created until after the
#'   \code{timestamp}.
#' @param x FlyWire rootids in any format understandable to
#'   \code{\link{ngl_segments}} including as \code{integer64}
#' @param timestamp (optional) argument to set an endpoint - edits after this
#'   time will be ignored (see details).
#' @inheritParams flywire_latestid
#' @param ... Additional arguments to \code{\link{flywire_fetch}}
#'
#' @return A logical vector of length matching the input. NA/0 input values will
#'   return NA as output.
#' @export
#' @family flywire-ids
#' @examples
#' \donttest{
#' flywire_islatest("720575940621039145")
#' flywire_islatest(c("720575940619073968", "720575940637707136"))
#' # check the first id up to a given timestamp, now TRUE
#' flywire_islatest("720575940619073968", timestamp = "2020-12-01")
#' }
#' \dontrun{
#' latest=flywire_latestid("720575940619073968")
#' flywire_islatest(latest)
#'
#' # compare checking roots downstream of two large bilateral neurons
#' blids=c("720575940619073968", "720575940637707136")
#' blidsout=flywire_partners(blids)
#' # 3.2 vs 4.7s in my test
#' bench::mark(bin=flywire_islatest(blidsout$post_id),
#'   str=flywire_islatest(as.character(blidsout$post_id)))
#' }
flywire_islatest <- function(x, cloudvolume.url=NULL, timestamp=NULL, ...) {
  url=flywire_api_url("is_latest_roots?int64_as_str=1", cloudvolume.url=cloudvolume.url)
  if(!is.null(timestamp)) {
    if(is.character(timestamp)) timestamp=as.POSIXct(timestamp, tz = '')
    url=sprintf("%s&timestamp=%d", url, as.integer(timestamp))
  }
  ids=if(is.integer64(x)) {
    x[is.na(x)]=0
    x
  } else ngl_segments(x, as_character = TRUE, must_work = F)
  if(length(ids)==0) {
    warning("no valid ids passed to flywire_islatest")
    return(logical())
  }
  # nb it takes as long to find unique ids as to find duplicates
  uids=unique(ids)
  # drop any 0s; setdiff munges bit64
  uids=uids[uids!=0L]
  if(length(uids)<length(ids)) {
    islatest <- if(length(uids)==0L) logical() else flywire_islatest(uids, ...)
    res <- islatest[match(x, uids)]
    return(res)
  }
  if(is.integer64(ids)) {
    url=paste0(url, "&is_binary=1")
    body=rids2raw(ids)
  } else {
    body=list(node_ids=I(ids))
  }
  res=flywire_fetch(url = url, body=body, ... )
  res$is_latest
}


#' Update root ids for flywire neurons using XYZ or supervoxel ids
#'
#' @param x Current root ids
#' @param svids optional supervoxel ids
#' @param xyz optional xyz locations in any form understood by
#'   \code{\link{xyzmatrix}}
#' @inheritParams  flywire_xyz2id
#' @param Verbose Whether to print a message to the console when updates are
#'   required.
#' @param ... Additional arguments passed to \code{\link{flywire_islatest}} or
#'   \code{\link{flywire_latestid}}
#'
#' @return
#' @export
#' @family flywire-ids
#' @examples
#'
#' kcs=data.frame(
#' rootid=c("720575940602553568", "720575940602564320", "720575940602605536"),
#' xyz=c("(159284,42762,3594)", "(159035,41959,3594)", "(157715,44345,3594)")
#' )
#' # update root ids
#' kcs$rootid=flywire_updateids(kcs$rootid, xyz=kcs$xyz)
flywire_updateids <- function(x, svids=NULL, xyz=NULL, rawcoords=FALSE,
                              voxdims=c(4,4,40), Verbose=TRUE, ...) {
  if(!is.null(xyz) && !is.null(svids)) {
    warning("only using svids for update!")
    xyz=NULL
  }
  if(is.null(xyz) && is.null(svids)) {
    warning("No xyz or svids argument. Falling back to (slow) flywire_latestid!")
    return(flywire_latestid(x, ...))
  }
  fil=flywire_islatest(x, ...)
  toupdate=!fil & !is.na(fil)
  if(!any(toupdate))
    return(x)

  newids <- if(!is.null(xyz)) {
    xyz=xyzmatrix(xyz)[toupdate,,drop=F]
    badrows=rowSums(is.na(xyz))>0
    if(any(badrows)) {
      xyz=xyz[!badrows, , drop=FALSE]
      toupdate=toupdate & !badrows
      warning("unable to update ", sum(badsvids), " ids with bad supervoxel info")
    }
    if(Verbose) message("Updating ", sum(toupdate), " ids")
    flywire_xyz2id(xyz, voxdims=voxdims, rawcoords=rawcoords)
  } else {
    badsvids=is.na(svids[toupdate])
    if(any(badsvids)) {
      toupdate=toupdate & !badsvids
      warning("unable to update ", sum(badsvids), " ids with bad supervoxel info")
    }
    if(Verbose) message("Updating ", sum(toupdate), " ids")
    flywire_rootid(svids[toupdate])
  }

  x[toupdate]=newids
  x
}
