decode_url <- function(u, ..., simplifyVector = TRUE, return.json=FALSE) {
  uu=utils::URLdecode(u)
  json=sub("[^{]+(\\{.*\\})$","\\1",uu)
  if(nchar(json)==nchar(uu))
    stop("I couldn't extract a JSON fragment from that URL")
  if(return.json) return(json)
  res=try(jsonlite::fromJSON(json, simplifyVector = simplifyVector, ...), silent = T)
  if(inherits(res,'try-error')){
    print(attr(res,'condition'))
    stop("Invalid JSON component in URL!")
  }
  res
}

#' Encode scene information into a neuroglancer URL
#'
#' @param body A text file or character vector with JSON data or an R list
#'   object
#' @param baseurl The base URL including the neuroglancer server
#' @inheritParams jsonlite::toJSON
#' @param ... Additional arguments for \code{\link[jsonlite]{toJSON}}
#'
#' @return Character vector containing encoded URL
#' @seealso \code{\link{URLencode}}, \code{\link{open_fafb_ngl}},
#'   \code{\link[jsonlite]{toJSON}}
#' @export
#'
#' @examples
#' \dontrun{
#' # copy JSON scene information from {} symbol at top right of neuroglancer
#' # now make a permanent URL for the scene
#' ngl_encode_url(clipr::read_clip())
#' }
ngl_encode_url <- function(body, baseurl=getOption("fafbseg.baseurl"),
                           auto_unbox=TRUE, ...) {
  json <- if(is.character(body)) {
    # if this looks like a file read it, otherwise assume it is json
    if(isTRUE(tools::file_ext(body)=='json')) readLines(body) else body
  } else {
    jsonlite::toJSON(body, auto_unbox=auto_unbox, ...)
  }
  json <- jsonlite::minify(json)
  # add extra chunk to url
  len=nchar(baseurl)
  if(!length(len) || len<2)
    stop("Invalid baseurl!")
  tail=substr(baseurl, len-2, len)
  if(!tail=='#!')
    baseurl=file.path(baseurl, "#!", fsep = "/")
  paste0(baseurl, utils::URLencode(json))
}

#' Construct Neuroglancer URL based on 3D location data
#'
#' @details Neuroglancer scenes seem to be specified in a single URL that
#'   URLencodes a json object defining layers to display, position etc. This
#'   function works by taking a sample URL defining such a scene and then
#'   editing it to point to a new 3D location / adjust zoom.
#'
#'   This package comes with a default scene url. If you specify a different
#'   sample URL to the \code{sampleurl} argument it will be remember for the
#'   rest of the R session. If you regularly use a particular kind of scene URL,
#'   you can set \code{options(fafbseg.sampleurl)} in your
#'   \code{\link{Rprofile}} file.
#'
#' @param x A numeric vector OR any object compatible with
#'   \code{\link[nat]{xyzmatrix}} OR a CATMAID URL (see details)
#' @param s Optional selection function of the type returned by
#'   \code{\link[rgl]{select3d}}
#' @param zoomFactor The Neuroglancer zoomFactor (bigger means zoomed out)
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
#' @examples
#' u=paste0("https://fafb.catmaid.virtualflybrain.org/?pid=2&zp=131280&",
#' "yp=170014.98879622458&xp=426584.81386896875&tool=navigator&sid0=2&s0=-1")
#'
#' # translate URL but don't open browser
#' open_fafb_ngl(u, open=FALSE)
#'
#' # produce an x,y,z string to paste into Neuroglancer
#' open_fafb_ngl(u, coords.only=TRUE)
#' \dontrun{
#' # copy CATMAID URL from clipboard and Neuroglancer coords to clipboard
#' clipr::write_clip(open_fafb_ngl(clipr::read_clip(), coords.only=TRUE))
#'
#' # Open a location in MB peduncle
#' open_fafb_ngl(c(433440, 168344, 131200))
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
open_fafb_ngl <- function(x, s = rgl::select3d(), zoomFactor=8, sampleurl=NULL,
                          coords.only=FALSE, open=interactive() && !coords.only,
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
  if(is.null(sampleurl)) {
    sampleurl=getOption('fafbseg.sampleurl',
                        stop("You must specify sampleurl at least once per R session!"))
  } else {
    options('fafbseg.sampleurl'=sampleurl)
  }
  # f=system.file('neuroglancer/split3xfill.json', package = 'fafbseg')
  # j=read_json(f, simplifyVector = T)
  j=decode_url(sampleurl)
  if(is.null(j$navigation$pose$position))
    stop("Sorry, this scene URL does not seem to have any navigation information!")
  j$navigation$pose$position$voxelCoordinates=as.vector(xyz/j$navigation$pose$position$voxelSize)
  j$navigation$zoomFactor=zoomFactor

  u <- if(coords.only) {
    paste(j$navigation$pose$position$voxelCoordinates, collapse = ',')
  } else ngl_encode_url(j)
  if(open) {
    browseURL(u)
    invisible(u)
  } else u
}
