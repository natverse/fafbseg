# Access to the graphene server used by flywire

#' GET/POST from flywire (graphene) servers with appropriate authorisation token
#'
#' @section authorisation: Your authorisation will be based on a chunked graph
#'   token normally stored at
#' \code{~/.cloudvolume/secrets/chunkedgraph-secret.json}.
#' For more details see article on \href{http://natverse.org/fafbseg/articles/articles/accessing-graphene-server.html}{accessing-graphene-server}.
#'
#' @importFrom httr add_headers
#' @inheritParams brainmaps_fetch
#' @param return One of "parsed", "text" (for raw JSON), or "response"
#' @param config (optional) curl options, see \code{httr::\link[httr]{config}}
#'   for details.
#'
#' @return Either an R object based on parsing returned JSON, a character vector
#'   containing the raw JSON or a \code{httr::\link[httr]{response}} object,
#'   depending on the value of \code{return}.
#' @export
#' @examples
#' \donttest{
#' # convert a flywire state URL into a parsed neuroglancer scene information
#' json=flywire_fetch("https://globalv1.flywire-daf.com/nglstate/5747205470158848",
#'   return="text")
#' ngl_segments(ngl_decode_scene(json), as_character = T)
#' }
flywire_fetch <- function(url,
                          body = NULL,
                          config = NULL,
                          return = c("parsed", "text", "response"),
                          cache = FALSE,
                          retry = 0L,
                          include_headers = FALSE,
                          simplifyVector = TRUE,
                          ...) {

  #Step 1: Identify the return type to be sent back..
  return=match.arg(return)

  #Step 2: Get configuration of the http request, so you can add the token there..
  if (is.null(config))
    config = httr::config()
  token = chunkedgraph_token()
  config = c(config, add_headers(Authorization = paste("Bearer", token)))

  #Step 3: choose the actual request function to use, if cache on try the memoised one
  # otherwise use the retry from httr..
  httpreq_fun <- if (cache) memoised_RETRY else httr::RETRY

  #Step 4: if body is present (and is not already in JSON format), convert that to JSON..
  hasbody <- !is.null(body)
  if (hasbody && !is.character(body))
    body = jsonlite::toJSON(body, auto_unbox = TRUE)

  #Step 5: set the number of retry attempts if retry is switched ON..
  if (isTRUE(retry))
    retry = 3L
  else if (isFALSE(retry))
    retry = 0L

  #Step 6: perform the actual http request to the server..
  req <- httpreq_fun(
    ifelse(hasbody, 'POST', "GET"),
    url = url,
    # config = config(Authorization=paste("Bearer", token)),
    config = config,
    body = body,
    times = retry,
    ...
  )

  #Step 7: Check/handle if there was an error in the message back from the request..
  flywire_errorhandle(req)

  #Step 8: Parse and return the type of data requested..
  if (return=='parsed') {
    parsed = parse_json(req, simplifyVector = simplifyVector)
    if (length(parsed) == 2 && isTRUE(names(parsed)[2] == 'error')) {
      stop("flywire error: " , parsed$error)
    }
    if (include_headers) {
      fields_to_include = c("url", "headers")
      attributes(parsed) = c(attributes(parsed), req[fields_to_include])
    }
    parsed
  } else if(return=="text") {
    httr::content(req, as='text', type = 'application/json', encoding = 'UTF-8')
  } else req
}

#' @importFrom httr http_error content message_for_status stop_for_status headers
flywire_errorhandle <- function(req) {
  # function to handle http errors from the flywire server..
  if(http_error(req)){
    ct=headers(req)[['content-type']]
    if(isTRUE(grepl("application/json", fixed = TRUE, ct))){
      errdetails=content(req, as="parsed", type="application/json")
      message_for_status(req)
      stop(errdetails$error$message)
    } else stop_for_status(req)
  }
}
