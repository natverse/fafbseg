#' Fetch data specified by a set of curl shell comands (e.g. from Chrome)
#'
#' @param x Path to a file or character vector of statements
#' @param outdir Optional output directory (will be created if necessary)
#' @param regex Optional regular expression that curl statements must match
#' @param ... Additional arguments to \code{\link{grep}}
#'
#' @return The path to the output directory invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_all_curl("all_curl.sh", outdir="alldata", regex="meshes:batch", fixed=TRUE)
#' }
fetch_all_curl <- function(x, outdir=NULL, regex="^curl", ...) {
  if(length(x)==1 && file.exists(x))
  if(!is.null(regex)) {
    x=grep(regex, x, value = TRUE, ...)
  }
  if(!length(x)){
    warning("Nothing to do!")
    return(invisible(NULL))
  }

  pb <- progress::progress_bar$new(
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
    f=sprintf("chunk%05d.raw", i)
    cmd=paste(cl, "-o", f)
    system(cmd, ignore.stdout = T, ignore.stderr = T)
    pb$tick()
  }
  invisible(outdir)
}
