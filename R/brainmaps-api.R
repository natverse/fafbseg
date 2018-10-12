#' GET/POST from brainmaps API
#'
#' @param url Full URL for brainmaps API endpoint
#' @param body an R list with parameters that will be converted with
#'   \code{jsonlite::\link{toJSON}} and then passed on to \code{\link{POST}}.
#'   You can also pass a \code{JSON} character vector to have more control of
#'   the \code{JSON} encoding.
#' @param simplifyVector Whether to use \code{jsonlite::simplifyVector}
#' @inheritParams catmaid::catmaid_fetch
#' @return An R list parse
#' @export
#' @importFrom httr GET POST stop_for_status with_config config
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' brainmaps_fetch("https://brainmaps.googleapis.com/v1/volumes")
#' }
brainmaps_fetch <- function(url, body=NULL, parse.json=TRUE,
                            include_headers=FALSE, simplifyVector=TRUE, ...) {
  google_token=brainmaps_auth()

  req<-with_config(config(token = google_token), {
    if(is.null(body)) {
      GET(url=url, ...)
    } else {
      if(!is.character(body))
        body=jsonlite::toJSON(body)
      POST(url=url, body=body, ...)
    }
  } )
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

#' @importFrom httr http_error content message_for_status
brainmaps_error_check <- function(req) {
  if(http_error(req)){
    errdetails=content(req, as="parsed", type="application/json")
    message_for_status(req)
    stop(errdetails$error$message)
  }
}

#' Convert 3D x,y,z locations in brainmaps volumes to segmentation ids
#'
#' @param xyz N x 3 matrix of points or an object containing vertex data that is
#'   compatible with \code{\link{xyzmatrix}}. These should be in physical space
#'   (i.e. nm) unless \code{voxdims=NULL}.
#' @param volume character vector identifier string for the volume containing
#'   segmentation data - see examples
#' @param voxdims the implied voxel dimensions for the volume. If set to
#'   \code{NULL} then the function will not attempt to scale the incoming x,y,z
#'   locations.
#' @param ... Additional arguments passed to \code{\link{brainmaps_fetch}}
#' @return A numeric vector of Google segment ids
#' @export
#' @examples
#' \dontrun{
#' # Physical location in nm
#' brainmaps_xyz2id(c(433368, 168208, 128480))
#' # Same location as displayed in neuroglancer
#' brainmaps_xyz2id(c(54171, 21026, 3212), voxdims = NULL)
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
#' }
brainmaps_xyz2id <- function(xyz,
                             volume="772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2",
                             voxdims = c(8, 8, 40),
                             ...) {
  baseurl="https://brainmaps.googleapis.com/"
  relurl=sprintf("v1/volumes/%s/values", volume)
  fullurl=file.path(baseurl, relurl)
  if(isTRUE(is.vector(xyz) && length(xyz)==3)) {
    xyz=matrix(xyz, ncol=3)
  } else {
    xyz=xyzmatrix(xyz)
  }
  if(!is.null(voxdims))
    xyz=scale(xyz, scale = voxdims, center = FALSE)
  xyz=round(xyz)
  mode(xyz)='integer'
  xyzstr=paste(xyz[,1],xyz[,2], xyz[,3], sep=',')
  body=list(locations=xyzstr)
  res=brainmaps_fetch(fullurl, body=body, ...)
  as.numeric(unlist(res, use.names = FALSE))
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
                                    volume="772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2",
                                    meshName="mcws_quad1e6", ...) {
  url <- sprintf("https://brainmaps.googleapis.com/v1/objects/%s/meshes/%s:listfragments",
                 volume, meshName)

  unlist(brainmaps_fetch(url, query=list(object_id=x), ...), use.names = F)
}

# Fetch fragment ids for a set of segment ids
segments2fragmentdf <- function(x, volume="772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2", meshName="mcws_quad1e6", ...) {
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
#'   be read)
#' @examples
#' \dontrun{
#' segs=find_merged_segments(7186840767)
#' samplemesh=read_brainmaps_meshes(segs)
#' sampleskel=read_segments2(segs)
#' dot3d(samplemesh, col='grey')
#' plot3d(sampleskel, lwd=2)
#' }
read_brainmaps_meshes <- function(x,
                                  volume="772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2",
                                  meshName="mcws_quad1e6", ...) {
  ff=brainmaps_batchmeshes(x, volume=volume, meshName=meshName, ...)
  yy=read_ng_raw(ff)
  unlink(ff)
  as.mesh3d(yy)
}

brainmaps_batchmeshes <- function(x, volume="772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2", meshName="mcws_quad1e6", ...) {

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
