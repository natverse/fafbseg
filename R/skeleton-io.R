#' Read skeletons for segments by extracting from the corresponding zip file(s)
#'
#' @param x A vector of segment ids or any Neuroglancer scene specification that
#'   includes segments ids (see examples and \code{\link{ngl_segments}} for
#'   details).
#' @param voxdims The voxel dimensions in nm of the skeletonised data
#' @param ... additional arguments passed to \code{\link[nat]{read.neurons}}
#' @return A \code{\link[nat]{neuronlist}} containing one
#'   \code{\link[nat]{neuron}} for each fragment
#' @seealso \code{\link[nat]{read.neurons}}, \code{\link{ngl_segments}},
#'   \code{\link{read_brainmaps_meshes}} to read 3D meshes.
#' @importFrom nat read.neurons
#' @export
#' @examples
#' \dontrun{
#' # read neuron using raw segment identifier
#' n <- read_segments2(22427007374)
#'
#' # read a neuron from a scene specification copied from Neuroglancer window
#' # after clicking on the {} icon at top right
#' n <- read_segments2(clipr::read_clip())
#'
#' summary(n)
#'
#' n2 <- read_segments2(22427007374, datafrac=0.9)
#' summary(n2)
#' }
read_segments <- function(x, voxdims=c(32,32,40), ...) {
  x=ngl_segments(x)
  # fl will be a list
  ff=character()
  for(seg in x) {
    skels=skelsforsegment(seg)
    ff=c(ff, extract_zip_files(skels))
  }
  # it will be useful to know the segment / fragment ids
  df=as.data.frame(swc2segmentid(ff, include.fragment = TRUE))
  rownames(df)=basename(ff)
  suppressMessages(suppressWarnings(read.neurons(ff, df = df, format='swc', ...)*voxdims))
}

#' @rdname read_segments
#' @param datafrac Fraction of the data to read based on uncompressed file size
#'   (see details)
#' @description \code{read_segments2} is a reworked version of
#'   \code{read_segments} that reads skeletons straight from zip files to
#'   memory.
#' @details I would recommend \code{read_segments2} at this point.
#'   \code{read_segments} has the potential benefit of caching SWC files on disk
#'   rather than extracting every time. However there is a large slowdown on
#'   many filesystems as the number of extracted files enters the thousands -
#'   something that I have hit a few times. Furthermore \code{read_segments2}
#'   makes it easier to select fragment files \emph{before} extracting them.
#'
#'   \code{datafrac} a number in the range 0-1 specifies a fraction of the data
#'   to read. Skeleton fragments will be placed in descending size order and
#'   read in until the number of bytes exceeds \code{datafrac} * sum(all file
#'   sizes). We have noticed that the time taken to read a neuron from a zip
#'   file seems to depend largely on the number of fragments that are read in,
#'   rather than the amount of data in each fragment! Reading 90% of the data
#'   can take < 10% of the time!
#'
#' @param minfilesize The uncompressed size of the swc file must be >= this. A
#'   cheap way to insist that we have >1 point.
#' @param coordsonly Only read in XYZ coordinates of neurons.
#' @export
#' @importFrom nat data.frame<-
read_segments2 <- function(x, voxdims=c(32,32,40), minfilesize=80,
                           datafrac=NULL, coordsonly=FALSE, ...) {
  zdf=skelsforsegments(x)
  zdf=zdf[zdf$uncompressed_size>=minfilesize,]

  if(!is.null(datafrac)) {
    if(datafrac<0 || datafrac>1)
      stop("invalid value of datafrac should be in range 0-1!")

    zdf=zdf[order(zdf$uncompressed_size, decreasing=TRUE),]
    totsize=sum(zdf$uncompressed_size)
    size_threshold=datafrac*totsize
    index_to_stop=min(which(cumsum(zdf$uncompressed_size) > size_threshold))
    zdf = zdf[seq_len(index_to_stop), ]
  }

  zdf=cbind(zdf, swc2segmentid(zdf$filename, include.fragment = T))

  rownames(zdf)=tools::file_path_sans_ext(zdf$filename)
  ff=zdf$filename
  names(ff)=tools::file_path_sans_ext(ff)
  res=nat::nlapply(ff, read.neuron.from.zip, voxdims=voxdims, coordsonly=coordsonly, ...)
  data.frame(res)=zdf
  res
}

read.coords.from.zip <- function(zip, file) {
  swc=read.swc.from.zip(zip, file)
  swc[,c("X","Y","Z")]
}

read.swc.from.zip <- function(zip, file){
  u=unz(zip, file, open='rb')
  on.exit(close(u))

  res=readr::read_delim(
    u, delim=" ",
    col_names = c("PointNo","Label","X","Y","Z","W","Parent"),
    col_types =
      readr::cols(
        PointNo = readr::col_integer(),
        Label = readr::col_integer(),
        X = readr::col_double(),
        Y = readr::col_double(),
        Z = readr::col_double(),
        W = readr::col_double(),
        Parent = readr::col_integer()
      )
  )
  res
}

read.neuron.from.zip <- function(file, voxdims=NULL, coordsonly=FALSE) {
  zip=segmentid2zip(swc2segmentid(file))
  zip=zip_path(zip, mustWork = TRUE)
  res=read.swc.from.zip(zip, file)
  # SWC format
  res$W=res$W*2
  # FIXME what units for the incoming radius? Raw or nm?
  if(!is.null(voxdims)) {
    cols=c("X","Y","Z","W")
    for(i in seq_along(voxdims)) {
      col=cols[i]
      res[[col]]=res[[col]]*voxdims[i]
    }
  }
  if(coordsonly) {
    res[,c("X","Y","Z")]
  } else nat::as.neuron(res, InputFileName=file)
}

#' Find all skeleton fragments for one segment
#'
#' @param x A segment id
#' @param returndetails Whether to return all zip list details
#' @return a character vector of fragment names with an attribute containing the
#'   path to the zip file
skelsforsegment <- function(x, returndetails=FALSE) {
  zip=segmentid2zip(x)
  zipp=zip_path(zip, mustWork = TRUE)
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

skelsforsegments <- function(x, returndetails=FALSE) {
  x=ngl_segments(x)

  pb <- progress::progress_bar$new(total = length(x), show_after=0.5,
    format = "  skelsforsegments [:bar] :percent eta: :eta")

  zl=sapply(x, function(...) {pb$tick();skelsforsegment(...)}, returndetails=TRUE, simplify = FALSE)
  dplyr::bind_rows(zl, .id='id')
}

#' Find and read the largest n segments from one or more skeleton zip files
#'
#' \code{read_topn} calls \code{find_topn} to find the top fragments and then
#' reads them in using \code{read_segments2}.
#'
#' @details Note that this will read all the fragment skeletons for each segment
#' @param zipfiles The path, name, or number of the zipfiles. If this is not a
#'   full path then it will be searched for in the location defined by
#'   options('fafbseg.skelziproot')
#' @param n Number of segments to read
#' @param ... additional arguments passed to \code{\link{read_segments2}}
#' @importFrom nat read.neurons
#' @importFrom dplyr group_by summarise arrange desc top_n
#' @export
#' @seealso \code{\link{read_segments}}
#' @examples
#' \dontrun{
#' top3=read_topn("224270.zip", n=3)
#' }
read_topn <- function(zipfiles, n=1, ...) {
  topsegments <- find_topn(zipfiles, n=n)
  read_segments2(topsegments$segment, ...)
}

#' @description \code{find_topn} finds the files, returning a \code{tibble}
#'   (\code{data.frame}) of results.
#' @importFrom dplyr mutate
#' @importFrom memoise forget
#' @param decreasing Whether to sort the skeletons in decreasing order i.e.
#'   largest first (default=\code{TRUE})
#' @export
#' @rdname read_topn
find_topn <- function(zipfiles, n=1, decreasing = TRUE) {
  if(is.numeric(zipfiles) || !all(file.exists(zipfiles))) {
    zipfiles=zip_path(zipfiles)
  }

    process1 <- function(zipfile){
    zl=zip_list(zipfile)
    pb$tick()
    zl$segment=swc2segmentid(zl$filename)
    zl %>%
      group_by(segment) %>%
      summarise(total_size=sum(uncompressed_size), nfragments=n()) %>%
      top_n(n, if(decreasing) total_size else -1.0*total_size) %>%
      arrange(if(decreasing) desc(total_size) else total_size) %>%
      mutate(zipfile=zipfile, seq=1:n())
  }

  pb <- progress::progress_bar$new(total = length(zipfiles), show_after=0.5,
    format = "  find_topn [:bar] :percent eta: :eta")

  ll=lapply(zipfiles, process1)
  dplyr::bind_rows(ll)
}
