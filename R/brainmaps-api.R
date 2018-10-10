#' GET/POST from brainmaps API
#'
#' @inheritParams catmaid::catmaid_fetch
#'
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
