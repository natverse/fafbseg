#' GET/POST from brainmaps API
#'
#' @param url Full URL for brainmaps API endpoint
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
      body=jsonlite::toJSON(body)
      POST(url=url, body=body, ...)
    }
  } )
  # error out if there was a problem
  stop_for_status(req)
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

#' Convert 3D x,y,z locations in brainmaps volumes to segmentation ids
#'
#' @param xyz Nx3 matrix of points or an object containing vertex data that is
#'   compatible with \code{\link{xyzmatrix}}. These should be in physical space
#'   (i.e. nm) unless \code{voxdims=NULL}.
#' @param volume character vector identifier string for the volume containing
#'   segmentation data - see examples
#' @param voxdims the implied voxel dimensions for the volume. If set to
#'   \code{NULL} then the function will not attempt to scale the incoming x,y,z
#'   locations.
#' @param ... Additional arguments passed to \code{\link{brainmaps_fetch}}
#' @return A numeric vector of Google segment ids
#' @examples
#' \dontrun{
#' brainmaps_xyz2id(c(54171, 21026,3212), voxdims = NULL)
#' brainmaps_xyz2id(c(433368, 168208, 128480))
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
