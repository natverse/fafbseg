# Access to the graphene server used by flywire

#' GET/POST from flywire (graphene) servers with appropriate authorisation token
#'
#' @section authorisation: Your authorisation will be based on a chunked graph
#'   token normally stored at
#'   \code{~/.cloudvolume/secrets/chunkedgraph-secret.json}. See
#'   https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson for the
#'   format. You will need to generate the token as advised by the FlyWire team.
#'   Search or ask for help \code{#help_software} in the FlyWire slack if you
#'   can't find the information.
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
  return=match.arg(return)
  if (is.null(config))
    config = httr::config()

  token = chunkedgraph_token()
  config = c(config, add_headers(Authorization = paste("Bearer", token)))
  FUN <- if (cache)
    mRETRY
  else
    httr::RETRY

  hasbody <- !is.null(body)
  if (hasbody && !is.character(body))
    body = jsonlite::toJSON(body, auto_unbox = TRUE)

  if (isTRUE(retry))
    retry = 3L
  else if (isFALSE(retry))
    retry = 0L

  req <- FUN(
    ifelse(hasbody, 'POST', "GET"),
    url = url,
    # config = config(Authorization=paste("Bearer", token)),
    config = config,
    body = body,
    times = retry,
    ...
  )
  # error out if there was a problem
  flywire_error_check(req)
  if (return=='parsed') {
    parsed = parse_json(req, simplifyVector = simplifyVector)
    if (length(parsed) == 2 && isTRUE(names(parsed)[2] == 'error')) {
      stop("catmaid error: " , parsed$error)
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
flywire_error_check <- function(req) {
  if(http_error(req)){
    ct=headers(req)[['content-type']]
    if(isTRUE(grepl("application/json", fixed = TRUE, ct))){
      errdetails=content(req, as="parsed", type="application/json")
      message_for_status(req)
      stop(errdetails$error$message)
    } else stop_for_status(req)
  }
}

# this is very much still WIP
read_graphene_meshes <- function(segment) {
  baseurl="https://fafbv2.dynamicannotationframework.com"
  manifesturl=sprintf("%s/meshing/1.0/fly_v31/manifest/%s:0?verify=True",
                      baseurl,
                      as.character(segment))
  manifest=graphene_fetch(manifesturl)
  if(!length(manifest$fragments))
    stop("No fragments to fetch!")
  manifest$fragments

  info=graphene_fetch(
    paste0(baseurl, "/segmentation/1.0/fly_v31/info"), cache = TRUE)

  if(!isTRUE(substr(info$data_dir, 1, 5)=="gs://"))
    stop("Cannot parse data directory for meshes!\n", info$data_dir)
  dd=sub("gs://", "https://storage.googleapis.com/", fixed=T,info$data_dir)
  basemeshurl=file.path(dd,info$mesh)

  l=list()
  pb <- progress_bar$new(
    format = "  downloading :what [:bar] :percent eta: :eta",
    total = length(manifest$fragments))

  for(frag in manifest$fragments) {
    pb$tick()
    meshurl=file.path(basemeshurl, frag)
    res=httr::GET(meshurl)
    httr::stop_for_status(res)
    l[[frag]]=httr::content(res, as = 'raw')
  }
  l
}
