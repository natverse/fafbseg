#' Fetch data specified by a set of curl shell comands (e.g. from Chrome)
#'
#' @details You can generate an appropriate set of commands by opening the
#'   Chrome Developer console (View ... Developer ... JavaScript Console),
#'   (re)loading a page of interest, selecting the network tab, selecting a
#'   downloaded object, right clicking and then choosing (Copy ... Copy all as
#'   cURL).
#'
#' @param x Path to a file or character vector of statements (by default it
#'   reads statements from the clipboard)
#' @param outdir Optional output directory (will be created if necessary)
#' @param regex Optional regular expression that curl statements must match
#' @param filename A \code{\link{sprintf}} style format statement that will be
#'   used to name the downloaded files.
#' @param ... Additional arguments to \code{\link{grep}}
#'
#' @return The path to the output directory invisibly
#' @export
#' @importFrom progress progress_bar
#'
#' @examples
#' \dontrun{
#' fetch_all_curl("all_curl.sh", outdir="alldata", regex="meshes:batch", fixed=TRUE)
#' }
fetch_all_curl <- function(x=clipr::read_clip(), outdir=NULL, regex="^curl",
                           filename="chunk%05d.raw", ...) {
  if(length(x)==1 && file.exists(x))
  if(!is.null(regex)) {
    x=grep(regex, x, value = TRUE, ...)
  }
  if(!length(x)){
    warning("Nothing to do!")
    return(invisible(NULL))
  }

  pb <- progress_bar$new(
    total = length(x),
    format = "  downloading [:bar] :percent eta: :eta")

  if(!is.null(outdir)) {
    if(!file.exists(outdir)) dir.create(outdir, recursive = TRUE)
    owd <- setwd(outdir)
    on.exit(setwd(owd))
  }

  i=0
  for(cl in x) {
    # remove terminal semi colon
    cl <- sub(";$","",cl)
    i=i+1
    f=sprintf(filename, i)
    cmd=paste(cl, "-o", f)
    system(cmd, ignore.stdout = T, ignore.stderr = T)
    pb$tick()
  }
  invisible(outdir)
}
