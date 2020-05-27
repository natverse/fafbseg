#' Decode scene information from Neuroglancer URL or JSON block
#'
#' @param x Character vector containing single Neuroglancer URL or a json block
#' @param return.json When \code{TRUE} extracts the JSON block in a URL does not
#'   parse it to an R list
#' @inheritParams jsonlite::fromJSON
#' @return An R list with additional class \code{ngscene} describing the scene,
#'   or, when \code{return.json=TRUE}, a character vector.
#' @export
#' @family neuroglancer-urls
#' @importFrom utils URLdecode
#' @seealso \code{\link[utils]{URLdecode}}, \code{\link[jsonlite]{fromJSON}}
#' @examples
#' \dontrun{
#' ngl_decode_scene("<someneuroglancerurl>")
#'
#' # decode scene from URL currently on clipboard
#' scene=ngl_decode_scene(clipr::read_clip())
#'
#' # open a Neuroglancer URL in CATMAID
#' ngu="<someurl>"
#' library(elmr)
#' open_fafb(ngl_decode_scene(ngu))
#' # Or store the URL rather than opening it
#' cmu=open_fafb(ngl_decode_scene(ngu), open=FALSE)
#' }
ngl_decode_scene <- function(x, return.json=FALSE, simplifyVector = TRUE,
                             simplifyDataFrame = FALSE, ...) {
  if(length(x)==1 && isTRUE(substr(x, 1, 4)=="http")) {
    # This looks like a Neuroglancer URL
    uu=URLdecode(x)
    x=sub("[^{]+(\\{.*\\})$","\\1",uu)
    if(nchar(x)==nchar(uu))
      stop("I couldn't extract a JSON fragment from that URL")
    if(return.json) return(x)
  }
  res=try(jsonlite::fromJSON(x, simplifyVector = simplifyVector,
                             simplifyDataFrame = simplifyDataFrame, ...), silent = T)
  if(inherits(res,'try-error')){
    stop("Invalid JSON scene description!\n",
         as.character(attr(res,'condition')))
  }
  class(res)=c('ngscene','list')
  res
}

#' @export
xyzmatrix.ngscene <- function(x, ...) {
  pos=x$navigation$pose$position
  if(is.null(pos)) stop("scene contains no position information")
  matrix(pos[['voxelCoordinates']]*pos$voxelSize, ncol=3)
}

#' Encode scene information into a neuroglancer URL
#'
#' @param body A text file or character vector with JSON data or an R list
#'   object
#' @param baseurl A URL specifying the neuroglancer server (if missing, uses
#'   \code{options("fafbseg.sampleurl")}). You can use any neuroglancer URL as
#'   will be appropriately truncated if it encodes scene information.
#' @param fix_segments Fix URL when only one segment in scene (see details)
#' @inheritParams jsonlite::toJSON
#' @param ... Additional arguments for \code{\link[jsonlite]{toJSON}}
#'
#' @details When the neuroglancer URL scene refers to just one segment the only
#'   way we can currently ensure correctly formed JSON is to add a dummy 0
#'   segment (thus forming a JSON array).
#'
#' @return Character vector containing encoded URL
#' @seealso \code{\link{URLencode}}, \code{\link{open_fafb_ngl}},
#'   \code{\link[jsonlite]{toJSON}}
#' @export
#' @family neuroglancer-urls
#' @examples
#' \dontrun{
#' # copy JSON scene information from {} symbol at top right of neuroglancer
#' # now make a permanent URL for the scene
#' ngl_encode_url(clipr::read_clip())
#' }
ngl_encode_url <- function(body, baseurl=NULL,
                           auto_unbox=TRUE, fix_segments=TRUE, ...) {
  json <- if(is.character(body)) {
    # if this looks like a file read it, otherwise assume it is json
    if(isTRUE(tools::file_ext(body)=='json')) readLines(body) else body
  } else {
    if(fix_segments && auto_unbox) {
      # pad and length 1 segment vectors with a 0 (which should be ignored)
      # to avoid a formatting error where auto_unbox produces a json scalar
      # when neuroglancer wants to see a json array
      bl=body[['layers']]
      fix_segment <- function(x) {
        xs=x[['segments']]
        if(length(xs)==1)
          x[['segments']]=c("0", xs)
        x
      }
      body[['layers']] <- lapply(bl, fix_segment)
    }
    jsonlite::toJSON(body, auto_unbox=auto_unbox, ...)
  }
  json <- jsonlite::minify(json)
  baseurl=baseurl_from_url(baseurl)
  paste0(baseurl, utils::URLencode(json))
}

#' Construct Neuroglancer URL based on 3D location data
#'
#' @details Neuroglancer scenes seem to be specified in a single URL that
#'   URLencodes a json object defining layers to display, position etc. This
#'   function works by taking a sample URL defining such a scene and then
#'   editing it to point to a new 3D location / adjust zoom.
#'
#'   This package comes with 4 different default scene urls specified via
#'   \code{\link{choose_segmentation}} or \code{\link{with_segmentation}}. This
#'   is the easiest way to choose a particular segmentation. You can also
#'   specify a different sample URL via the \code{sampleurl} argument; it will
#'   be remembered for the rest of the R session. If you regularly use a
#'   particular kind of scene URL, you can set \code{options(fafbseg.sampleurl)}
#'   in your \code{\link{Rprofile}} file.
#'
#' @param x A numeric vector OR any object compatible with
#'   \code{\link[nat]{xyzmatrix}} OR a CATMAID URL (see details)
#' @param s Optional selection function of the type returned by
#'   \code{\link[rgl]{select3d}}
#' @param zoomFactor The Neuroglancer zoomFactor (bigger means zoomed out)
#' @param sample,reference Template space of the input object \code{sample} and
#'   target (\code{reference}). See examples and \code{\link{xform_brain}} for
#'   details of how these are specified.
#' @param sampleurl A sample URL that defines your Neuroglancer dataset.
#' @param coords.only Return raw coordinate string for pasting into Neuroglancer
#'   position widget (top left of screen)
#' @param open Whether or not to open the URL in a browser - this defaults to
#'   \code{TRUE} in interactive use.
#' @param ... Additional arguments passed to \code{\link[jsonlite]{fromJSON}} to
#'   control JSON parsing.
#' @return A character vector with a Neuroglancer URL or coordinate string
#'   (invisibly when \code{open=TRUE})
#' @importFrom jsonlite read_json
#' @importFrom catmaid catmaid_parse_url
#' @importFrom utils browseURL
#' @export
#' @family neuroglancer-urls
#' @examples
#' u=paste0("https://fafb.catmaid.virtualflybrain.org/?pid=2&zp=131280&",
#' "yp=170014.98879622458&xp=426584.81386896875&tool=navigator&sid0=2&s0=-1")
#'
#' # translate URL but don't open browser
#' open_fafb_ngl(u, open=FALSE)
#'
#' # produce an x,y,z string to paste into Neuroglancer
#' open_fafb_ngl(u, coords.only=TRUE)
#'
#' # translate URL converting from FAFB14 to FlyWire coordinates
#' # (only a small shift)
#' open_fafb_ngl(u, sample="FAFB14", reference="FlyWire", open=FALSE)
#' \dontrun{
#' # copy CATMAID URL from clipboard and Neuroglancer coords to clipboard
#' clipr::write_clip(open_fafb_ngl(clipr::read_clip(), coords.only=TRUE))
#'
#' # Open a location in MB peduncle with current preferred segmentation
#' open_fafb_ngl(c(433440, 168344, 131200))
#'
#' # choose a particular segmentation (Google FAFB)
#' with_segmentation("20190805", open_fafb_ngl(c(433440, 168344, 131200),
#' zoomFactor=2))
#'
#' # or FlyWire
#' with_segmentation("flywire", open_fafb_ngl(c(433440, 168344, 131200)))
#'
#' # ... and translate FAFB14 to FlyWire coordinates
#' with_segmentation("flywire", open_fafb_ngl(c(433440, 168344, 131200),
#' sample="FAFB14", reference="FlyWire", zoomFactor=2))
#'
#' # open a CATMAID URL in Neuroglancer
#' open_fafb_ngl(u)
#'
#' # Set an existing scene URL (pointing to any old location) to act as
#' # the template for open_fafb_ngl
#' # nb the package sets one for you on startup if you haven't set yourself
#' options(fafbseg.sampleurl="https://<neuroglancerlurl>")
#' # Edit your R profile if you want to set a different default
#' usethis::edit_r_profile()
#' }
#' @importFrom nat.templatebrains xform_brain
open_fafb_ngl <- function(x, s = rgl::select3d(), zoomFactor=8,
                          coords.only=FALSE, open=interactive() && !coords.only,
                          sample=NULL, reference=NULL, sampleurl=NULL,
                          ...) {
  if(is.character(x)) {
    x=catmaid_parse_url(x)
  }
  if (is.vector(x, mode = "numeric") && length(x) == 3) {
    xyz = matrix(x, ncol = 3)
  }
  else {
    xyz = xyzmatrix(x)
    if (nrow(xyz) > 1) {
      xyz = colMeans(xyz[s(xyz), , drop = F])
      xyz = matrix(xyz, ncol = 3)
    }
  }
  if(!is.null(sample) && !is.null(reference))
    xyz=xform_brain(xyz, sample=sample, reference = reference, ...)

  sampleurl=check_sampleurl(sampleurl)

  # f=system.file('neuroglancer/split3xfill.json', package = 'fafbseg')
  # j=read_json(f, simplifyVector = T)
  j=ngl_decode_scene(sampleurl)
  if(is.null(j$navigation$pose$position))
    stop("Sorry, this scene URL does not seem to have any navigation information!")
  j$navigation$pose$position$voxelCoordinates=as.vector(xyz/j$navigation$pose$position$voxelSize)
  j$navigation$zoomFactor=zoomFactor

  u <- if(coords.only) {
    paste(j$navigation$pose$position$voxelCoordinates, collapse = ',')
    # nb make sure that we use a base URL matching the sample URL we were given
  } else ngl_encode_url(j, baseurl = baseurl_from_url(sampleurl))
  if(open) {
    browseURL(u)
    invisible(u)
  } else u
}


# helper function to make base url from sample URL
baseurl_from_url <- function(url=NULL,
                                    fragment='!') {
  # use sampleurl option if unset, but never set the option
  url <- check_sampleurl(url, set = FALSE)
  pu <- httr::parse_url(url)
  pu$path=NULL
  pu$fragment <- if(isTRUE(nzchar(fragment))) fragment else NULL
  baseurl <- httr::build_url(pu)
  baseurl
}

check_sampleurl <- function(sampleurl=NULL, set=NA) {
  op=getOption('fafbseg.sampleurl')
  if(is.null(sampleurl)) {
    if(is.null(op))
      stop("You must specify sampleurl at least once per R session",
           ", using choose_segmentation() or in your .Rprofile!")
    sampleurl=op
  } else {
    # we were passed a sampleurl argument and option was unset
    if(!isFALSE(set) && is.null(op)) {
      if(interactive())
        message(sprintf("Setting options(fafbseg.sampleurl='%s')", sampleurl))
      options('fafbseg.sampleurl'=sampleurl)
    }
  }
  sampleurl
}

check_cloudvolume_url <- function(cloudvolume.url=NULL, set=NA) {
  op=getOption('fafbseg.cloudvolume.url')
  if(is.null(cloudvolume.url)) {
    if(is.null(op))
      stop("You must specify cloudvolume.url at least once per R session",
           ", using choose_segmentation() or in your .Rprofile!")
    cloudvolume.url=op
  } else {
    # we were passed a cloudvolume.url argument and option was unset
    if(!isFALSE(set) && is.null(op)) {
      if(interactive())
        message(sprintf("Setting options(fafbseg.cloudvolume.url='%s')", cloudvolume.url))
      options('fafbseg.cloudvolume.url'=cloudvolume.url)
    }
  }
  cloudvolume.url
}
