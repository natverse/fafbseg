#' GET/POST from brainmaps API with optional retry / cache
#'
#' @description \code{brainmaps_fetch} calls the brainmaps API with optional
#'   request cache and retries.
#'
#' @param url Full URL for brainmaps API endpoint
#' @param body an R list with parameters that will be converted with
#'   \code{jsonlite::\link{toJSON}} and then passed on to \code{\link{POST}}.
#'   You can also pass a \code{JSON} character vector to have more control of
#'   the \code{JSON} encoding.
#' @param simplifyVector Whether to use \code{jsonlite::simplifyVector}
#' @param cache Whether or not to cache responses (default \code{FALSE})
#' @param retry The number of times to retry the operation (default 0,
#'   \code{FALSE}=>\code{0} and \code{TRUE}=>3). See the documentation of the
#'   \code{times} argument of \code{httr::\link{RETRY}} for further details.
#' @param ... additional arguments passed to the \code{httr::{RETRY}} function.
#'   This may include a \code{\link[httr]{config}} list other named parameters
#'   etc.
#' @inheritParams catmaid::catmaid_fetch
#' @return An R list parse
#' @export
#' @importFrom httr GET POST stop_for_status with_config config
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' brainmaps_fetch("https://brainmaps.googleapis.com/v1/volumes")
#' # retry up to 4 times on failure
#' brainmaps_fetch("https://brainmaps.googleapis.com/v1/volumes", retry=4)
#' # cache results
#' brainmaps_fetch("https://brainmaps.googleapis.com/v1/volumes", cache=TRUE)
#' # use arbitrary curl/httr options (see httr_options())
#' httr::with_config(httr::verbose(), {
#'   brainmaps_fetch("https://brainmaps.googleapis.com/v1/volumes")
#' })
#' }
brainmaps_fetch <- function(url, body=NULL, parse.json=TRUE,
                            cache=FALSE, retry=0L,
                            include_headers=FALSE, simplifyVector=TRUE, ...) {
  google_token=brainmaps_auth()

  hasbody <- !is.null(body)
  if(hasbody && !is.character(body))
    body=jsonlite::toJSON(body, auto_unbox = TRUE)

  httpreq_fun <- if(cache) memoised_RETRY else httr::RETRY

  if(isTRUE(retry)) retry=3L
  else if(isFALSE(retry)) retry=0L

  req <- httpreq_fun(
    ifelse(hasbody, 'POST', "GET"),
    url = url,
    config = config(token = google_token),
    body = body,
    times = retry,
    ...
  )
  # error out if there was a problem
  brainmaps_error_check(req)
  if(parse.json) {
    parsed=parse_json(req, simplifyVector=simplifyVector)
    if(length(parsed)==2 && isTRUE(names(parsed)[2]=='error')) {
      stop("catmaid error: " , parsed$error)
    }
    if(include_headers) {
      fields_to_include=c("url", "headers")
      attributes(parsed) = c(attributes(parsed), req[fields_to_include])
    }
    parsed
  } else req
}

memoised_RETRY <- memoise::memoise(httr::RETRY)


#' @description \code{brainmaps_clear_cache} clears the cache used by \code{brainmaps_fetch}
#' @export
#'
#' @examples
#' \donttest{
#' brainmaps_clear_cache()
#' }
#' @rdname brainmaps_fetch
brainmaps_clear_cache <- function() {
  memoise::forget(memoised_RETRY)
}

#' @importFrom jsonlite fromJSON
#' @importFrom httr content
parse_json <- function(req, simplifyVector = FALSE, ...) {
  text <- content(req, as = "text", encoding = "UTF-8")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  fromJSON(text, simplifyVector = simplifyVector, ...)
}


#' Generate a token to enable access to brainmaps API
#'
#' @details You will need to activate the brainmaps API, generate a project at
#'   \url{https://console.developers.google.com} and then request OAuth
#'   credentials, which will create a client id and client secret. The
#'   application name on the console must be \code{fafbseg}. You should do this
#'   once and then put the keys in your \code{\link{.Renviron}} file.
#'
#'   \itemize{
#'
#'   \item{\code{BRAINMAPS_CLIENT_ID="xxxx"}}
#'
#'   \item{\code{BRAINMAPS_CLIENT_SECRET="xxxx"}}
#'
#'   }
#' @param client_id,client_secret Client id and secret copied from
#'   \url{https://console.developers.google.com}. See details.
#' @param scope The scope for the brainmaps API - you shouldn't need to change
#'   this.
#'
#' @return A token that can be used with e.g. \code{\link[httr]{GET}}.
#' @export
#' @seealso See \code{\link{.Renviron}} for how to set environment variables
#' @importFrom httr oauth_app oauth2.0_token oauth_endpoints
#' @examples
#' \dontrun{
#' google_token=brainmaps_auth()
#' # get a list of available volumes
#' req <- GET("https://brainmaps.googleapis.com/v1beta2/volumes",
#'   config(token = google_token))
#' }
brainmaps_auth <- function(client_id=Sys.getenv("BRAINMAPS_CLIENT_ID"),
                           client_secret=Sys.getenv("BRAINMAPS_CLIENT_SECRET"),
                           scope="https://www.googleapis.com/auth/brainmaps") {
  myapp <- oauth_app("fafbseg",
                     key = client_id,
                     secret = client_secret)
  google_token <- oauth2.0_token(oauth_endpoints("google"),
                                 myapp,
                                 scope = scope)
  google_token
}

#' @importFrom httr http_error content message_for_status stop_for_status headers
brainmaps_error_check <- function(req) {
  if(http_error(req)){
    ct=headers(req)[['content-type']]
    if(isTRUE(grepl("application/json", fixed = TRUE, ct))){
      errdetails=content(req, as="parsed", type="application/json")
      message_for_status(req)
      stop(errdetails$error$message)
    } else stop_for_status(req)
  }
}

#' Convert 3D x,y,z locations in brainmaps volumes to segmentation ids
#'
#' @details The underlying \code{brainmaps} API expects raw coordinates i.e.
#'   voxel indices. This is slightly complicated by the fact that different
#'   segmentation, skeleton etc volumes may have different associated voxel
#'   dimensions. \code{brainmaps_xyz2id} automatically looks up this voxel
#'   dimension.
#'
#'   However it may be that you want to pass in raw coordinates from
#'   neuroglancer. These will generally be associated with a the resolution of
#'   the image data not e.g. skeletons which may have a larger (coarser) voxel
#'   size. As a convenience in this situation you can set \code{rawcoords=TRUE}.
#'
#'   If you have problems with requests failing sporadically, it may be helpful
#'   to know \itemize{
#'
#'   \item Google does not keep the underlying data live so there may be some
#'   spin-up time.
#'
#'   \item there is an arbitrary timeout at the Google end (currently ~ 5s)
#'
#'   \item batching nearby points helps
#'
#'   }
#'
#'   Using the \code{chunksize} argument or the \code{retry} argument passed on
#'   to \code{\link{brainmaps_fetch}} can overcome these issues. See the
#'   examples.
#'
#' @param xyz N x 3 matrix of points or an object containing vertex data that is
#'   compatible with \code{\link{xyzmatrix}}. These should be in physical space
#'   (i.e. nm) unless \code{voxdims=NULL}.
#' @param volume character vector identifier string for the volume containing
#'   segmentation data - by default it uses the value of the
#'   \code{fafbseg.skeletonuri} option. Any input that can be parsed by
#'   \code{\link{brainmaps_volume}} is acceptable.
#' @param rawcoords Whether the coordinates are voxel indices (when
#'   \code{rawcoords=TRUE}) or physical units (nm, the default).
#' @param rawvoxdims the implied voxel dimensions for the volume. If
#'   \code{rawcoords=TRUE} then this will be used to convert the raw coordinates
#'   into physical units. The default value matches the normal voxel size in
#'   neuroglancer. If \code{rawvoxdims=NULL} then no attempt is made to scale
#'   the coordinates whatsoever. See details.
#' @param chunksize send queries in batches each of which has at most
#'   \code{chunksize} points. The default is chosen since the brainmaps API can
#'   time out if the points take too long to map (more likely if they are spread
#'   out across the brain). There is also a maximum number of points per call
#'   (10,000 was the recommended upper limit at one point).
#' @param ... Additional arguments passed to \code{\link{brainmaps_fetch}}. This
#'   can include setting the \code{retry} argument to >0. See \bold{Details} and
#'   \bold{Examples}.
#' @return A numeric vector of Google segment ids
#' @export
#' @examples
#' \dontrun{
#' # Physical location in nm
#' brainmaps_xyz2id(c(433368, 168208, 128480))
#' # Same location as displayed in neuroglancer
#' brainmaps_xyz2id(c(54171, 21026, 3212), rawcoords=TRUE)
#'
#' # Raw coodinates for the brainmaps volume in question - don't touch
#' brainmaps_xyz2id(c(433368, 168208, 128480)/c(32,32,40), rawvoxdims=NULL)
#'
#' library(elmr)
#' # get a manually traced neuron (just keep first and only entry in neuronlist)
#' dl4=read.neurons.catmaid('glomerulus DL4 right')[[1]]
#' # map every node location to segmentation ids
#' dl4.segs=brainmaps_xyz2id(dl4)
#' # remove unmapped locations which get id 0
#' dl4.segs=setdiff(dl4.segs, 0)
#' # read in corresponding skeletons
#' dl4.skels=read_segments2(dl4.segs)
#' # read in corresponding skeletons after including agglomeration merge groups
#' dl4.allskels=read_segments2(find_merged_segments(dl4.segs))
#'
#' ## retries / cache / chunksize issues
#' # set small chunk size
#' dl4.segs=brainmaps_xyz2id(dl4, chunksize=500)
#' # use retries in case of failure
#' dl4.segs=brainmaps_xyz2id(dl4, chunksize=500, retry=3)
#' # cache successful requests (if you might need to repeat)
#' dl4.segs=brainmaps_xyz2id(dl4, chunksize=500, retry=3, cache=TRUE)
#' }
brainmaps_xyz2id <- function(xyz,
                             volume=getOption('fafbseg.skeletonuri'),
                             rawcoords=FALSE,
                             rawvoxdims = c(8, 8, 40),
                             chunksize=getOption('fafbseg.brainmaps_xyz2id.chunksize', 200),
                             ...) {
  baseurl="https://brainmaps.googleapis.com/"
  # extract well formatted volume id
  volume=brainmaps_volume(volume)
  relurl=sprintf("v1/volumes/%s/values", volume)
  fullurl=file.path(baseurl, relurl)
  if(isTRUE(is.vector(xyz) && length(xyz)==3)) {
    xyz=matrix(xyz, ncol=3)
  } else {
    xyz=xyzmatrix(xyz)
  }
  voxdims=brainmaps_voxdims(volume, ...)
  if(rawcoords){
    voxdims=voxdims/rawvoxdims
  }
  if(!is.null(rawvoxdims))
    xyz=scale(xyz, scale = voxdims, center = FALSE)
  xyz=round(xyz)
  mode(xyz)='integer'

  brainmaps_call <- function(xyz, fullurl) {
    xyzstr=paste(xyz[,1],xyz[,2], xyz[,3], sep=',')
    body=list(locations=xyzstr)
    res=brainmaps_fetch(fullurl, body=body, ...)
    pb$tick(nrow(xyz))
    res
  }

  nx=nrow(xyz)
  nchunks=ceiling(nx/chunksize)
  chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nx)]
  chunkstoread=seq_len(nchunks)
  res=list()
  pb <- progress::progress_bar$new(total = nx, show_after=0.5,
    format = "  brainmaps_xyz2id [:bar] :percent eta: :eta")

  for(i in chunkstoread) {
    res[[length(res)+1]]=brainmaps_call(xyz[chunks==i,,drop=F], fullurl)
  }
  as.numeric(unlist(res, use.names = FALSE))
}

brainmaps_geometry <- function(volume, cache=TRUE, ...) {
  volume=brainmaps_volume(volume)
  baseurl="https://brainmaps.googleapis.com/v1/volumes/%s"
  res=brainmaps_fetch(sprintf(baseurl, volume), cache=cache, ...)
  res[[1]]
}

brainmaps_voxdims <- function(volume, ...) {
  checkmate::assert_character(volume, len=1)
  geom=brainmaps_geometry(volume, ...)
  checkmate::assert_data_frame(geom[['pixelSize']], min.rows = 1)
  unlist(geom[['pixelSize']][1,], use.names = FALSE)
}

#' Low level call to brainmaps API to list mesh fragment ids for segment ids
#'
#' @param x Single segment identifier
#' @inheritParams read_brainmaps_meshes
#'
#' @return Character vector of fragment ids
#' @export
#' @seealso \code{\link{read_brainmaps_meshes}}
#'
#' @examples
#' \dontrun{
#' brainmaps_listfragments(7186840767)
#' }
brainmaps_listfragments <- function(x,
                                    volume=getOption("fafbseg.brainmaps.volume"),
                                    meshName=getOption("fafbseg.brainmaps.meshName"),
                                    ...) {
  url <- sprintf("https://brainmaps.googleapis.com/v1/objects/%s/meshes/%s:listfragments",
                 volume, meshName)

  unlist(brainmaps_fetch(url, query=list(object_id=x), ...), use.names = F)
}

# Fetch fragment ids for a set of segment ids
segments2fragmentdf <- function(x,
                                volume=getOption("fafbseg.brainmaps.volume"),
                                meshName=getOption("fafbseg.brainmaps.meshName"),
                                ...) {
  pb <- progress::progress_bar$new(total = length(x), show_after=0.5,
    format = "  brainmaps_listfragments [:bar] :percent eta: :eta")

  res <- sapply(x,
                function(x, ...) {
                  pb$tick()
                  lf = brainmaps_listfragments(x, ...)
                  dplyr::data_frame(object_id = x, fragment_key = lf)
                },
                volume = volume,
                meshName = meshName,
                ...,
                simplify = FALSE)
  names(res) = x
  dplyr::bind_rows(res)
}

segments2batches <- function(x, chunksize=100, ...) {
  df=segments2fragmentdf(x, ...)
  res=make_batches_chunked(df, chunksize=chunksize)
  res
}


#' Read 3D meshes via the brainmaps API
#'
#' @param x Vector of integer segment ids
#' @param volume String identifier for the volume containing segments
#' @param meshName String identifier for the meshes
#' @param ... Additional arguments passed to \code{\link{brainmaps_fetch}}
#'
#' @return A \code{\link[rgl]{mesh3d}} object
#' @export
#' @seealso \code{\link{read_segments2}} to read skeleton fragments and
#'   \code{\link{brainmaps_listfragments}} (to identify the fragments that must
#'   be read). See \code{\link{compare_ng_neuron}} to compare skeletons and 3D
#'   meshes.
#' @examples
#' \dontrun{
#' segs=find_merged_segments(7186840767)
#' samplemesh=read_brainmaps_meshes(segs)
#' sampleskel=read_segments2(segs)
#' dot3d(samplemesh, col='grey')
#' plot3d(sampleskel, lwd=2)
#'
#' # or compare mesh and skeleton colouring points by distance from skeleton
#' compare_ng_neuron(samplemesh, sampleskel, pointsize=1, sample_dots = 0.3)
#' }
read_brainmaps_meshes <- function(x,
                                  volume=getOption("fafbseg.brainmaps.volume"),
                                  meshName=getOption("fafbseg.brainmaps.meshName"),
                                  ...) {
  ff=brainmaps_batchmeshes(x, volume=volume, meshName=meshName, ...)
  yy=read_ng_raw(ff)
  unlink(ff)
  as.mesh3d(yy)
}

brainmaps_batchmeshes <- function(x,
                                volume=getOption("fafbseg.brainmaps.volume"),
                                meshName=getOption("fafbseg.brainmaps.meshName"),
                                ...) {
  if(!is.list(x)) {
    batches <- segments2batches(x)
    return(sapply(batches, brainmaps_batchmeshes, volume=volume, meshName=meshName, ...))
  } else batches <- x

  body=list(volume_id=volume,
            mesh_name=meshName,
            batches=batches)
  body=jsonlite::toJSON(body, auto_unbox = TRUE)
  res = brainmaps_fetch(
    "https://brainmaps.googleapis.com/v1/objects/meshes:batch",
    body = body,
    parse.json = FALSE,
    ...
  )
  tf <- tempfile(fileext = '.raw')
  writeBin(content(res, as='raw'), con = tf)
  tf
}

make_batch <- function(object_id, fragment_keys) {
  list(object_id = as.character(unique(object_id)),
               fragment_keys = fragment_keys)
}

make_batches <- function(fragmentdf) {
  res=by(fragmentdf,
         fragmentdf[['object_id']],
         function(x) make_batch(x[[1]], x[[2]]),
         simplify = F)
  class(res)='list'
  names(res)=NULL
  attributes(res)=NULL
  res
}

make_batches_chunked <- function(fragmentdf, chunksize=100) {
  nchunks=ceiling(nrow(fragmentdf)/chunksize)
  chunks=rep(seq_len(nchunks), rep(chunksize, nchunks))[seq_len(nrow(fragmentdf))]
  res=by(fragmentdf, chunks, make_batches, simplify = FALSE)
  class(res)='list'
  names(res)=NULL
  attributes(res)=NULL
  res
}

#' Read skeleton(s) from brainmaps API into a \code{nat::neuron} object
#'
#' @param ... Additional arguments passed to \code{\link{brainmaps_skeleton}}
#'   and then on to \code{\link{brainmaps_fetch}}. These may include a
#'   \code{skeletonuri} argument, a brainmaps URI specifying the remote source
#'   of the skeletons. See \code{\link{brainmaps_skeleton}} for details.
#' @inheritParams read_segments
#'
#' @return a \code{nat::\link[nat]{neuron}} object
#' @export
#'
#' @examples
#' \dontrun{
#' n=read.neuron.brainmaps(22427007374)
#' nm=read.neurons.brainmaps(find_merged_segments(7186840767))
#'
#' # you would specify a particular skeleton source like so
#' read.neurons.brainmaps(find_merged_segments(7186840767),
#'    skeletonuri="brainmaps://<volume>/<meshName>")
#' }
#' @importFrom checkmate assert_number
#' @importFrom nat ngraph as.neuron
read.neuron.brainmaps <- function(x, ...) {
  x=ngl_segments(x)
  res=brainmaps_skeleton(x, ...)
  # edge indices come in 0 offset
  ng=ngraph(res$edges+1, vertexnames = seq_len(res$nvertices), xyz = res$vertices)
  as.neuron(ng, id=as.numeric(x))
}

#' @rdname read.neuron.brainmaps
#' @inheritParams catmaid::read.neuron.catmaid
#' @export
#' @details When \code{OmitFailures} is not \code{NA}, \code{FUN} will be
#'   wrapped in a call to \code{\link{try}} to ensure that failure for any
#'   single neuron does not abort the \code{\link{nlapply}} call. When
#'   \code{OmitFailures=TRUE} the resultant \code{\link{neuronlist}} will be
#'   subsetted down to return values for which \code{FUN} evaluated
#'   successfully. When \code{OmitFailures=FALSE}, "try-error" objects will be
#'   left in place. In either of the last 2 cases error messages will not be
#'   printed because the call is wrapped as \code{try(expr, silent=TRUE)}.
#'
#'   The optional dataframe (\code{df}) detailing each neuron should have
#'   \code{rownames} that match the names of each neuron. It would also make
#'   sense if the same key was present in a column of the data frame. If the
#'   dataframe contains more rows than neurons, the superfluous rows are dropped
#'   with a warning. If the dataframe is missing rows for some neurons an error
#'   is generated. If \code{SortOnUpdate=TRUE} then updating an existing
#'   \code{\link{neuronlist}} should result in a new \code{\link{neuronlist}}
#'   with ordering identical to reading all neurons from scratch.
read.neurons.brainmaps<-function(x, OmitFailures=NA, df=NULL, ... ) {
  x=ngl_segments(x)
  if(is.null(df)) {
    names(x)=as.character(x)
    df=data.frame(segment=x,
                  stringsAsFactors = F)
    rownames(df)=names(x)
  } else {
    names(x)=rownames(df)
  }
  fakenl=nat::as.neuronlist(as.list(x), df=df)
  nat::nlapply(fakenl, read.neuron.brainmaps, OmitFailures=OmitFailures, ...)
}


#' Low level function to fetch skeleton from brainmaps API
#'
#' @details This API seems to return skeletons in physical (i.e. already
#'   calibrated) coordinates.
#'
#' @param x A single segment id
#' @param skeletonuri The brainmaps URI describing the skeleton source. Defaults
#'   to the value of \code{options("fafbseg.skeletonuri")}.
#' @param ... Additional arguments passed to \code{\link{brainmaps_fetch}}
#'
#' @return A list containing the following fields \itemize{
#'
#'   \item \code{nvertices} The number of vertices (n)
#'
#'   \item \code{nedges} The number of edges (m)
#'
#'   \item \code{vertices} A \code{n} x 3 matrix of vertex locations
#'
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' brainmaps_skeleton(9208128833)
#' brainmaps_skeleton(9208128833,
#'   skeletonuri=paste0("brainmaps://772153499790:fafb_v14:",
#'   "fafb_v14_16nm_v00c_split3xfill2_flatreseg2_skeletons32",
#'   "/teasar512_nnconn165_mc10000_prune10_thresh1000_sparse250"))
#' }
brainmaps_skeleton <- function(x, skeletonuri=getOption("fafbseg.skeletonuri"), ...) {
  assert_number(x, lower=1, upper=1.9E19, finite=TRUE)
  baseurl="https://brainmaps.googleapis.com/"
  checkmate::assert_character(skeletonuri)
  uril=parse_brainmaps_uri(skeletonuri, mesh_required = TRUE)
  relurl = sprintf("v1/objects/%s/meshes/%s/skeleton:binary",
                   uril$volume,
                   uril$meshName)
  fullurl=file.path(baseurl, relurl)
  res=brainmaps_fetch(fullurl, body=list(object_id=as.character(x)), parse.json = F, ...)
  rawbin=httr::content(res, type='raw')
  sizes=readBin(rawbin, 'integer', n=2, size=8)
  nbytes_sizes=2*8
  nbytes_vertices=sizes[1]*3*4
  vertices=readBin(rawbin[-(1:16)], 'numeric', n=sizes[1]*3, size=4)
  edges=readBin(rawbin[-seq_len(nbytes_sizes+nbytes_vertices)],
                   what='integer', n=sizes[2]*2, size=4)
  c(list(
    nvertices = sizes[1],
    nedges = sizes[2],
    vertices = matrix(vertices, ncol = 3, byrow = TRUE),
    edges = matrix(edges, ncol = 2, byrow = TRUE)
  ), uril)
}

parse_brainmaps_uri <- function(x, mesh_required=FALSE) {
  checkmate::assert_character(x, min.chars = nchar("brainmaps://")+1, len=1)
  res=stringr::str_match(x, 'brainmaps://([^/]+)(/([^/]+)){0,1}')
  resl=list(volume=res[,2], meshName=res[,4])
  if(is.na(resl$volume))
    stop("URI does not contain a volume")
  if(is.na(resl$meshName)) {
    if(isTRUE(mesh_required))
      stop("URI does not contain a mesh specifier")
    resl$meshName=NULL
  }
  class(resl)='brainmaps_uri'
  resl
}


#' Extract brainmaps volume identifier
#'
#' @param x character vector containing brainmaps URI, a parsed
#'   \code{brainmaps_uri} object or a character vector already containing a
#'   volume specifier.
#' @return character vector containing a volume specifier.
#' @export
#' @examples
#' brainmaps_volume("772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2")
#' brainmaps_volume("brainmaps://772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2")
brainmaps_volume <- function(x) {
  if(inherits(x, 'brainmaps_uri')){
    x=x[['volume']]
  } else
  checkmate::assert_character(x, pattern = ":", len = 1L)

  if(isTRUE(grepl(x, pattern = "^brainmaps://"))) {
    parse_brainmaps_uri(x)[['volume']]
  } else {
    x
  }
}
