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
#' n <- read_segments(22427007374)
#' }
read_segments <- function(x, voxdims=c(32,32,40), ...) {
  # fl will be a list
  ff=character()
  for(seg in x) {
    skels=skelsforsegment(seg)
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
#' @param returndetails Whether to return all zip list details
#' @return a character vector of fragment names with an attribute containing the
#'   path to the zip file
skelsforsegment <- function(x, returndetails=FALSE) {
  zip=segmentid2zip(x)
  zipp=zip_path(zip)
  zl=zip_list_m(zipp)
  if(returndetails) {
    m=grep(paste0("^", x,"\\."), zl[['filename']], useBytes = T, perl=T)
    matches=zl[m,]
  } else {
    matches=grep(paste0("^", x,"\\."), zl[['filename']], useBytes = T, value = T, perl=T)
  }
  attr(matches,'zip')=zipp
  matches
}

#' Read the largest n segments from a skeleton zip file
#'
#' @details Note that this will read all the fragments for these segments
#' @param zipfile The path, name, or number of the zipfile. If this is not a
#'   full path then it will be searched for in the location defined by
#'   options('fafbseg.skelziproot')
#' @param n Number of segments to read
#' @param ... additional arguments passed to \code{\link{read_segments}}
#' @importFrom nat read.neurons
#' @importFrom dplyr group_by summarise arrange desc top_n
#' @export
#' @seealso \code{\link{read_segments}}
#' @examples
#' \dontrun{
#' top3=read_topn("224270.zip", n=3)
#' }
read_topn <- function(zipfile, n=1, ...) {
  if(is.numeric(zipfile) || !file.exists(zipfile)) {
    zipfile=zip_path(zipfile)
  }
  zl=zip::zip_list(zipfile)
  zl$segment=swc2segmentid(zl$filename)
  topsegments <- zl %>%
    group_by(segment) %>%
    summarise(total_size=sum(uncompressed_size)) %>%
    top_n(n) %>%
    arrange(desc(total_size))
  read_segments(topsegments$segment, ...)
}
