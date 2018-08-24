#' Read skeletons for segments by extracting from the corresponding zip file(s)
#'
#' @param x A vector of segment ids (you might get these from a neuroglancer
#'   scene)
#' @param voxdims The voxel dimensions in nm of the skeletonised data
#' @param ... additional arguments passed to \code{\link[nat]{read.neurons}}
#' @return A \code{\link[nat]{neuronlist}} containing one
#'   \code{\link[nat]{neuron}} for each fragment
#' @seealso \code{\link[nat]{read.neurons}}
#' @importFrom nat read.neurons
#' @export
#' @examples
#' \donttest{
#' n <- read_segments(7574605868)
#' }
read_segments <- function(x, voxdims=c(32,32,40), ...) {
  # fl will be a list
  ff=character()
  for(seg in x) {
    skels=skelsforsegment(x)
    ff=c(ff, extract_zip_files(skels))
  }
  # If we keep on memoising zip lists, we'll quickly use a lot of memory ...
  memoise::forget(zip_list_m)
  # it will be useful to know the segment / fragment ids
  df=as.data.frame(swc2segmentid(ff, include.fragment = TRUE))
  rownames(df)=basename(ff)
  suppressMessages(suppressWarnings(read.neurons(ff, df = df, format='swc', ...)*voxdims))
}

#' Find all skeleton fragments for one segment
#'
#' @param x A segment id
#' @return a character vector of fragment names with an attribute containing the
#'   path to the zip file
skelsforsegment <- function(x) {
  zip=segmentid2zip(x)
  zipp=zip_path(zip)
  zl=zip_list_m(zipp)
  matches=grep(paste0("^", x,"\\."), zl[['filename']], useBytes = T, value = T, perl=T)
  attr(matches,'zip')=zipp
  matches
}

#' @importFrom nat read.neurons
read_topn <- function(zipfile, n=1, exdir=tempfile(), ...) {
  if(!file.exists(exdir)) {
    dir.create(exdir)
    on.exit(unlink(exdir, recursive = T))
  }
  pp=character()
  for(z in zipfile) {
    zl=zip::zip_list(z)
    idx=order(zl$uncompressed_size, decreasing = TRUE)
    selidx=idx[seq_len(n)]
    p=unzip(z, files = zl$filename[selidx], exdir = exdir, junkpaths=T)
    # FIXME, but probably tiny
    pp=c(pp, p)
  }
  read.neurons(pp, ...)*c(32,32,40)
}