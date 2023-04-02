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
    res=urldecode(httr::build_url(pu))
  }
  res
}


#' @description \code{flywire_expandurl} expands shortened URLs into a full
#'   neuroglancer JSON scene specification. If the active segmentation
#'   (\code{\link{choose_segmentation}}) is a flywire segmentation then that is
#'   used to define the initial part of the output URL, otherwise the
#'   \code{flywire31} segmentation is used.
#'
#'   \code{flywire_expandurl} will also expand tinyurl.com URLs.
#' @param json.only Only return the JSON fragment rather than the neuroglancer
#'   URL
#' @export
#'
#' @examples
#' \donttest{
#' flywire_expandurl("https://globalv1.flywire-daf.com/nglstate/5747205470158848")
#' flywire_expandurl("https://tinyurl.com/rmr58jpn")
#' }
#' @rdname flywire_shortenurl
flywire_expandurl <- function(x, json.only=FALSE, cache=TRUE, ...) {
  checkmate::assert_character(x, pattern="^http[s]{0,1}://")
  if(length(x)>1) {
    res=pbapply::pbsapply(x, flywire_expandurl, json.only=json.only, cache=cache, ...)
    return(res)
  }
  if(grepl("tinyurl.com", x, fixed = TRUE)) {
    # head should redirect to expanded URL
    x=httr::HEAD(x)$url
    if(json.only)
      x=ngl_decode_scene(x, return.json = TRUE)
    return(x)
  }

  if(isFALSE(su <- shorturl(x)))
    stop("This doesn't look like a shortened neuroglancer URL: ", x)
  x=flywire_fetch(su, cache=cache, return='text', ...)

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
#'   data.frame containing a column \code{rootid}, \code{root_id}, \code{id} or
#'   any form acceptable to \code{\link{ngl_segments}} including neuroglancer
#'   scene URLs.
#' @param open Whether to open the scene in your default browser
#' @param annotations data.frame or matrix of position and other information for
#'   annotation layers. See \code{\link{ngl_annotation_layers}} for details.
#' @param shorten Not currently implemented
#' @param ... Passed to \code{\link{ngl_annotation_layers}}
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
#' # using the ability to query flytable for cell types
#' flywire_scene('DA2_lPN', open=TRUE)
#' flywire_scene('class:MBON_R', open=TRUE)
#' }
flywire_scene <- function(ids=NULL, annotations=NULL, open=FALSE, shorten=FALSE, ...) {
  sc=with_segmentation("flywire", ngl_blank_scene())
  if(!is.null(ids)) {
    ngl_segments(sc) <- flywire_ids(ids, unique=TRUE)
  }
  if(!is.null(annotations))
    sc=sc+ngl_annotation_layers(annotations, ...)

  u=ngl_encode_url(sc)
  if(shorten)
    u=flywire_shortenurl(u)
  if(isTRUE(open)) {
    browseURL(u)
    invisible(u)
  } else u
}


#' Flexible specification of flywire ids (including from flytable types)
#'
#' @description allows more flexible specification of flywire root ids compared
#'   with \code{\link{ngl_segments}} including by queries against cell types
#'   recorded in flytable.
#'
#' @param x A character or bit64::integer64 vector or a dataframe specifying ids
#'   directly \emph{or} a string specifying a query (see examples) or a URL.
#' @param integer64 Whether to return ids as 64 bit integers - more compact than
#'   character vector, but can be more fragile (default \code{FALSE}).
#' @param check_latest Whether to check if ids are up to date.
#' @param must_work Whether ids must be valid
#' @param na_ok whether NA ids are acceptable when \code{must_work=TRUE}
#' @param unique Whether to return only unique ids
#' @param table When \code{x} is a query whether to search \code{brain},
#'   \code{optic} lobe or \code{both} info tables.
#' @inheritParams flywire_timestamp
#' @param ... Additional arguments passed to \code{\link{flytable_cell_types}}
#'   or \code{\link{ngl_segments}}.
#'
#' @return character (or \code{integer64})) vector of segment ids
#' @family neuroglancer-urls
#' @seealso \code{\link{flytable_cell_types}}.
#' @export
#'
#' @examples
#' flywire_ids(data.frame(root_id=1))
#' flywire_ids(data.frame(root_id=1), integer64=TRUE)
#' # BA
#' flywire_ids(data.frame(root_id=-1))
#' \dontrun{
#' # will error
#' flywire_ids(data.frame(root_id=-1), must_work = TRUE)
#' }
#' # DL1 olfactory PNs
#' flywire_ids("DL1_adPN")
#' # DL1 olfactory PNs but only on the RHS
#' flywire_ids("DL1_adPN_R")
#' # specifying materialisation version
#' flywire_ids("DL1_adPN_R", version=400)
#' # using SQL wild cards
#' flywire_ids("DA[12]_%PN_L")
#'
#' # all sensory neurons
#' flywire_ids("super:sensory", integer64=TRUE)
#'
#' # note that side is defined by soma position (not arbour side)
#' flywire_ids("class:MBON_R", integer64=TRUE)
#' # superclass can also have a side specified
#' flywire_ids("super:motor_R", integer64=TRUE)
flywire_ids <- function(x, integer64=FALSE, check_latest=FALSE, must_work=FALSE,
                        na_ok=FALSE, unique=FALSE, version=NULL,
                        table=c('both', 'info', 'optic'), ...) {
  if(is.data.frame(x)) {
    poss_cols=c("rootid", "root_id", 'flywire.id', 'flywire_id', 'id')
    cwh=intersect(poss_cols, colnames(x))
    if(length(cwh)>0) {
      x=x[[cwh[1]]]
    } else {
      i64=sapply(x, bit64::is.integer64)
      if(sum(i64)==1) {
        message("assuming that column ", colnames(x)[i64], " contains flywire ids!")
        x=x[[which(i64)]]
      }
    }
  } else if(is.character(x) && length(x)==1 && !valid_id(x, na.ok = T) && !grepl("http", x)) {
    # looks like a query
    target='type'
    if(grepl("^[a-z_]+:", x)) {
      okfields=c('type', 'cell_type', 'cell_class', 'hemibrain_type', 'class',
                 "super_class", "super", "ito_lee_hemilineage")
      ul=unlist(strsplit(x, ":", fixed=T))
      if(length(ul)!=2)
        stop("Unable to parse flywire id specification!")
      target=ul[1]
      if(!target %in% okfields)
        stop("Unknown field in flywire id specification!")
      if(target=='class')
        target='cell_class'
      if(target=='super')
        target='super_class'
      x=ul[2]
    }
    res=flytable_cell_types(pattern=x, target = target, version=version, table=table, ...)
    x=bit64::as.integer64(res$root_id)
  }
  if(!is.integer64(x))
    x=ngl_segments(x, must_work = must_work, unique = unique, ...)
  else {
    if(must_work && !all(valid_id(x, na.ok = na_ok)))
      stop("There are invalid ids.")
    if(unique) {
      ux=unique(x)
      if(length(ux)<length(x)) {
        warning("flywire_ids: Dropping ", length(x) - length(ux),
                " duplicate ids", call. = F)
        x=ux
      }
    }
  }
  x <- if(integer64) as.integer64(x) else as.character(x)
  if(check_latest)
    stopifnot(all(flywire_islatest(x, version=version, ...)))
  x
}
