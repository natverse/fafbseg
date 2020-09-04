#'Summarise edits for one or more Flywire segment ids
#'
#'@details When \code{filtered=TRUE} the following are removed \itemize{
#'
#'  \item edits that are outside of the given root id - meaning these edits are
#'  in the history of the cell but the affected part of the arbour was since cut
#'  off.
#'
#'  \item edits that were since undone by an inverse operation (often temporary
#'  splits made while reviewing a neuron.)
#'
#'  }
#'
#'@param x One or more flywire segment ids (character vector strongly
#'  recommended but any format accepted by \code{\link{ngl_segments}} will
#'  work).
#'@param root_ids Whether to look up the root ids before/after each change.
#'  Default \code{FALSE}. NB this is quite resource intensive so please do not
#'  flood the server with requests of this sort.
#'@param filtered Whether to filter out edits unlikely to relate to the current
#'  state of the neuron (default \code{TRUE}, see details).
#'@param tz Time zone for edit timestamps. Defaults to "UTC" i.e. Universal
#'  Time, Coordinated. Set to "" for your current timezone. See
#'  \code{\link{as.POSIXct}} for more details.
#'@param ... Additional arguments passed to \code{\link{flywire_fetch}}
#'
#'@return A data frame with values itemize{
#'
#'  \item{operation_id}{ a unique id for the edit}
#'
#'  \item{timestamp}{ in POSIXct format, to the nearest ms}
#'
#'  \item{user_id}{ numeric id for the user responsible for the edit}
#'
#'  \item{is_merge}{ whether it was a merge or a split}
#'
#'  \item{user_name}{ as a string}
#'
#'  }
#'
#'  In addition when \code{filtered=FALSE}, \code{in_neuron} \code{is_relevant}
#'
#'  In addition when \code{root_ids=TRUE}, \code{before_root_ids}
#'  \code{after_root_ids}, as space separated strings.
#'
#'
#'@export
#'
#' @examples
#' \dontrun{
#' flywire_change_log("720575940619010932")
#' flywire_change_log("720575940619010932", root_ids = TRUE)
#'flywire_change_log("720575940619010932", root_ids = TRUE)
#' }
flywire_change_log <- function(x, root_ids=FALSE, filtered=TRUE, tz="UTC", ...) {
  x=flywire_segments(x)
  if(length(x)>1) {
    res=pbapply::pbsapply(x, flywire_change_log, ..., simplify = FALSE)
    return(dplyr::bind_rows(res, .id='id'))
  }
  pu=httr::parse_url("https://prodv1.flywire-daf.com/segmentation/api/v1/table/fly_v31/root/%s/tabular_change_log")
  pu
  pu$path=sprintf(pu$path, x)
  pu$query=list(root_ids=as.character(root_ids),
                filtered=as.character(filtered))
  url=httr::build_url(pu)
  res=flywire_fetch(url, ...)
  if(isTRUE(root_ids)) {
    res[['before_root_ids']]=sapply(res[['before_root_ids']], paste,
                                    collapse=" ", USE.NAMES = F)
    res[['after_root_ids']]=sapply(res[['after_root_ids']], paste,
                                   collapse=" ", USE.NAMES = F)
  }
  df=as.data.frame(lapply(res, unlist))
  df$user_id=as.integer(df$user_id)
  df$timestamp=as.POSIXct(df$timestamp/1e3, origin="1970-01-01", tz=tz)
  df
}

# ids should be integers >= 0
# this does not check that they are also valid 64 bit ints which might be good
valid_id <- function(x, strict=TRUE) {
  grepl("^[0-9]+$", as.character(x))
}

# private helper function
# TODO merge into ngl_segments
flywire_segments <- function(x, as_character=TRUE, include_hidden=FALSE, cache=TRUE, ...) {
  if(length(x)>1 || is.numeric(x) || valid_id(x)) {
    # assume we have things that look like numbers already
  } else {
    # an URL?
    x=flywire_expandurl(x, cache=cache, ...)
    x=ngl_decode_scene(x)
  }
  ngl_segments(x, as_character=as_character, include_hidden=include_hidden)
}

# private helper function
# FIXME either put this into ngl_decode_scene or decide to export
flywire_expandurl <- function(x, json.only=FALSE, cache=TRUE, ...) {
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
    x=with_segmentation('flywire31', ngl_encode_url(x))
  }
  x
}
