# Access to the graphene server used by flywire

#' GET/POST from flywire (graphene) servers with appropriate authorisation token
#'
#' @section authorisation: Your authorisation will be based on a chunked graph
#'   token normally stored at
#'   \code{~/.cloudvolume/secrets/cave-secret.json}. See
#'   \url{https://github.com/seung-lab/cloud-volume#cave-secretjson} for
#'   the format. You will need to generate the token as advised by the FlyWire
#'   team. Search or ask for help \code{#help_software} in the FlyWire slack if
#'   you can't find the information. For more details see article on
#'   \href{http://natverse.org/fafbseg/articles/articles/accessing-graphene-server.html}{accessing-graphene-server}.
#'
#'
#' @importFrom httr add_headers
#' @inheritParams brainmaps_fetch
#' @param return One of "parsed", "text" (for raw JSON), or "response"
#' @param token Optional chunkedgraph token (otherwise the default one for the
#'   current segmentation will be used). Use \code{NA} to suppress use of a
#'   token.
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
#' # but see also flywire_expandurl
#' json=flywire_fetch("https://globalv1.flywire-daf.com/nglstate/5747205470158848",
#'   return="text")
#' ngl_segments(ngl_decode_scene(json), as_character = TRUE)
#' }
flywire_fetch <- function(url,
                          body = NULL,
                          config = NULL,
                          token=NULL,
                          return = c("parsed", "text", "response"),
                          cache = FALSE,
                          retry = 0L,
                          include_headers = FALSE,
                          simplifyVector = TRUE,
                          ...) {

  #Step 1: Identify the return type to be sent back..
  return=match.arg(return)

  # trim leading middleauth spec off URL
  url <- sub("^middleauth\\+", "", url)

  #Step 2: Get configuration of the http request, so you can add the token there..
  if (is.null(config))
    config = httr::config()
  if(is.null(token))
    token = chunkedgraph_token(url=url)
  if(!isTRUE(is.na(token)))
    config = c(config, add_headers(Authorization = paste("Bearer", token)))

  # 2a workaround for libcurl
  if(isTRUE(libcurl_version()=="8.7.1")) {
    # this specific version seems to have a bug with deflate/gzip encoding
    config=c(config, httr::add_headers(`Accept-Encoding`='none'))
    warn_hourly("Disabling gzip/deflate encoding due to buggy libcurl 8.7.1.\n",
                "You may want to update libcurl if possible.\n",
                "See https://github.com/curl/curl/issues/13493.")
  }

  #Step 3: choose the actual request function to use, if cache on try the memoised one
  # otherwise use the retry from httr..
  httpreq_fun <- if (cache) memoised_RETRY else httr::RETRY

  #Step 4: if body is present (and is not already in JSON format), convert that to JSON..
  hasbody <- !is.null(body)
  if (hasbody && !is.character(body) && !is.raw(body))
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
    if(isTRUE(req$headers$`content-type`=="data.arrow")) {
      check_package_available('arrow')
      parsed=arrow::read_ipc_stream(req$content)
    } else {
      # default is json
      parsed = parse_json(req, simplifyVector = simplifyVector, bigint_as_char=TRUE)
      if (length(parsed) == 2 && isTRUE(names(parsed)[2] == 'error')) {
        stop("flywire error: " , parsed$error)
      }
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

#' @importFrom httr http_error content message_for_status stop_for_status
#'   headers
flywire_errorhandle <- function(req) {
  # function to handle http errors from the flywire server..
  if(http_error(req)){
    ct=headers(req)[['content-type']]
    if(isTRUE(grepl("application/json", fixed = TRUE, ct))){
      errdetails=content(req, as="parsed", type="application/json")
      message_for_status(req)
      stop(unlist(errdetails))
    } else stop_for_status(req)
  }
}

libcurl_version <- memoise::memoise(function() {
  if(!requireNamespace('curl', quietly = TRUE)) return(NA_character_)
  else curl::curl_version()$version
}, cache = cachem::cache_mem(max_age = 600))

