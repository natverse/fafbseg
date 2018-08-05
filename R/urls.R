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

encode_url <- function(body, baseurl='https://neuroglancer-demo.appspot.com/#!', auto_unbox=TRUE, ...) {
  json <- if(is.character(body) && isTRUE(tools::file_ext(body)=='json')) {
    readLines(body)
  } else {
    jsonlite::toJSON(body, auto_unbox=auto_unbox, ...)
  }
  json <- jsonlite::minify(json)
  paste0(baseurl, utils::URLencode(json))
}

#' Construct Neuroglancer URL based on 3D location data
#'
#' @details Neuroglancer scenes seem to be specified in a single URL that
#'   URLencodes a json object definining layers to display, position etc. This
#'   function works by taking a sample URL defining such a scene and then
#'   editing it to point to a new 3D location / adjust zoom. You only need to
#'   specify the sample URL once per R session. This approach avoids having to
#'   construct these scene URLs from scratch or embedding the information within
#'   the package. If you regularly use a particular kind of scene URL, you can
#'   set \code{options(fafbseg.sampleurl)} in your \code{\link{Rprofile}} file.
#'
#' @param x A numeric vector OR any object compatible with
#'   \code{\link[nat]{xyzmatrix}} OR a CATMAID URL (see details)
#' @param s Optional selection function of the type returned by
#'   \code{\link[rgl]{select3d}}
#' @param zoomFactor The Neuroglancer zoomFactor (bigger means zoomed out)
#' @param sampleurl A sample URL that defines your neuroglancer dataset.
#' @param open Whether or not to open the URL in a browser - this defaults to
#'   \code{TRUE} in interactive use.
#' @param ... Additional arguments passed to \code{\link[jsonlite]{fromJSON}} to
#'   control JSON parsing.
#'
#' @importFrom jsonlite read_json
#' @importFrom catmaid catmaid_parse_url
#' @importFrom utils browseURL
#' @examples
#' \dontrun{
#' # Set an existing scene URL (pointing to any old location) to act as
#' # the template for open_fafb_ngl
#' options(fafbseg.sampleurl="https://<neuroglancerlurl>")
#'
#' # Open a location in MB peduncle
#' open_fafb_ngl(c(433440, 168344, 131200))
#'
#' # open a CATMAID URL in Neuroglancer
#' u=paste0("https://fafb.catmaid.virtualflybrain.org/?pid=2&zp=131280&",
#' "yp=170014.98879622458&xp=426584.81386896875&tool=navigator&sid0=2&s0=-1")
#' open_fafb_ngl(u)
#' }
open_fafb_ngl <- function(x, s = rgl::select3d(), zoomFactor=8, sampleurl=NULL, open=interactive(), ...) {
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
  u=encode_url(j)
  if(open) browseURL(u)
  invisible(u)
}
