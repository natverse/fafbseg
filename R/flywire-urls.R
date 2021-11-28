## Private (for now) helper functions

flywire_cloudvolume_url <- function(cloudvolume.url=NULL, graphene=TRUE) {
  if(is.null(cloudvolume.url)) {
    u=getOption("fafbseg.cloudvolume.url")
    # the current option points to a graphene URL (should be flywire or zetta) so use that
    cloudvolume.url <- if(grepl("graphene", u, fixed = TRUE))
      u else
        with_segmentation('flywire', getOption("fafbseg.cloudvolume.url"))
  }
  if(isTRUE(graphene)) cloudvolume.url
  else sub("graphene://", "", cloudvolume.url, fixed = T)
}


#' Shorten or expand neuroglancer URLs
#'
#' @description \code{flywire_shortenurl} makes short URLs from long URLs or
#'   \code{\link{ngscene}} objects that you may have constructed in R.
#' @param x One or more neuroglancer URLs or (for flywire_expandurl)
#'   \code{\link{ngscene}} parsed scene description.
#' @param include_base Whether to return a full URL that will open a
#'   neuroglancer session (the default) or only the component that defines the
#'   scene (which would display JSON in your browser).
#' @param baseurl Optional URL defining the neuroglancer browser to use with
#'   shortened URLs.
#' @param cache Whether to cache any calls to the flywire state server
#'   shortening or expanding URLs. Default is \code{TRUE}. NB this cache is only
#'   active for the current session.
#' @param ... Additional arguments passed to \code{\link{pbsapply}} (when
#'   multiple URLs to process) and then to \code{\link{ngl_encode_url}} (when
#'   generating a short URL for an \code{ngscene} list object) \emph{or} to
#'   \code{flywire_fetch} when using \code{flywire_expandurl}.
#'
#' @return A character vector containing one or more URLs.
#' @export
#'
#' @examples
#' \dontrun{
#' sc=ngl_blank_scene()
#' short=flywire_shortenurl(sc)
#' long=flywire_expandurl(short)
#' }
flywire_shortenurl <- function(x, include_base=TRUE, baseurl=NULL, cache=TRUE, ...) {
  if(is.ngscene(x)) {
    sc <- x
    x <- ngl_encode_url(sc, ...)
  } else {
    stopifnot(is.character(x))
    if(length(x)>1) {
      res=pbapply::pbsapply(x, flywire_shortenurl, include_base=include_base, baseurl=baseurl, cache=cache, ...)
      return(res)
    }
    sc=ngl_decode_scene(x)
  }
  state_server=sc$jsonStateServer
  if(is.null(state_server)) {
    state_server="https://globalv1.flywire-daf.com/nglstate/post"
    warning("Using default state server: ", state_server)
  }
  # get the json fragment
  json=ngl_decode_scene(x, return.json = TRUE)
  res=flywire_fetch(state_server, body = json, cache = cache)
  if(include_base) {
    baseurl=baseurl_from_url(baseurl)
    # baseurl="https://ngl.flywire.ai/?json_url="
    pu=httr::parse_url(baseurl)
    # We don't want the little #! that introduces a json fragment in a full URL
    pu$fragment=NULL
    pu$query=list(json_url=res)
    # build_url URLencodes, but this makes the result harder to read
    res=utils::URLdecode(httr::build_url(pu))
  }
  res
}


#' @description \code{flywire_expandurl} expands shortened URLs into a full
#'   neuroglancer JSON scene specification. If the active segmentation
#'   (\code{\link{choose_segmentation}}) is a flywire segmentation then that is
#'   used to define the initial part of the output URL, otherwise the
#'   \code{flywire31} segmentation is used.
#'
#' @param json.only Only return the JSON fragment rather than the neuroglancer
#'   URL
#' @export
#'
#' @examples
#' \donttest{
#' flywire_expandurl("https://globalv1.flywire-daf.com/nglstate/5747205470158848")
#' }
#' @rdname flywire_shortenurl
flywire_expandurl <- function(x, json.only=FALSE, cache=TRUE, ...) {
  checkmate::assert_character(x, pattern="^http[s]{0,1}://")
  if(length(x)>1) {
    res=pbapply::pbsapply(x, flywire_expandurl, json.only=json.only, cache=cache, ...)
    return(res)
  }
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
    # if we have a flywire segmentation active use that to encode URL
    flywire_active=isTRUE(grepl('flywire.ai', getOption('fafbseg.sampleurl')))
    x <- if (flywire_active)
      ngl_encode_url(x)
    else
      with_segmentation('flywire31', ngl_encode_url(x))
  }
  x
}


# return the base
flywire_api_url <- function(endpoint="", cloudvolume.url=NULL) {
  cloudvolume.url <- flywire_cloudvolume_url(cloudvolume.url, graphene = FALSE)
  url=sub("table", "api/v1/table", cloudvolume.url)
  lastchar=substr(url, nchar(url),nchar(url))
  if(lastchar!="/") url=paste0(url, "/")
  if(nzchar(endpoint)) paste0(url, endpoint) else url
}

#' Return a sample Neuroglancer scene URL for FlyWire dataset
#'
#' @param ids A set of root ids to include in the scene. Also accepts a
#'   data.frame containing a column \code{rootid}, \code{flywire.id}, \code{id}
#'   or any form acceptable to \code{\link{ngl_segments}} including neuroglancer
#'   scene URLs.
#' @param open Whether to open the scene in your default browser
#' @return A character vector containing a single Neuroglancer URL (invisibly
#'   when open=TRUE)
#' @export
#' @family neuroglancer-urls
#' @examples
#' \dontrun{
#' flywire_scene(open=T)
#' # top 20 partners of a neuron
#' flywire_scene(flywire_partner_summary("720575940621039145", partners='out')$partner[1:20], open=T)
#'
#' }
flywire_scene <- function(ids=NULL, open=FALSE) {
  sc=with_segmentation("flywire", ngl_blank_scene())
  if(!is.null(ids)) {
    ngl_segments(sc) <- flywire_ids(ids, unique=TRUE)
  }
  u=ngl_encode_url(sc)
  if(isTRUE(open)) {
    browseURL(u)
    invisible(u)
  } else u
}

# private function to extract ids
flywire_ids <- function(x, ...) {
  if(is.data.frame(x)) {
    if("rootid" %in% colnames(x)) x=x[['rootid']]
    else if("flywire.id" %in% colnames(x)) x=x[['flywire.id']]
    else if("flywire_id" %in% colnames(x)) x=x[['flywire_id']]
    else if("id" %in% colnames(x)) x=x[['id']]
    else {
      i64=sapply(x, bit64::is.integer64)
      if(sum(i64)==1) {
        message("assuming that column ", colnames(x)[i64], " contains flywire ids!")
        x=x[[which(i64)]]
      }
    }
  }
  ngl_segments(x, ...)
}
