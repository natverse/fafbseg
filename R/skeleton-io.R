#' Read skeletons for segments by extracting from the corresponding zip file(s)
#'
#' @param x A vector of segment ids or any Neuroglancer scene specification that
#'   includes segments ids (see examples and \code{\link{ngl_segments}} for
#'   details).
#' @param voxdims The voxel dimensions in nm of the skeletonised data
#' @param ... additional arguments passed to \code{\link[nat]{read.neurons}}
#' @return A \code{\link[nat]{neuronlist}} containing one
#'   \code{\link[nat]{neuron}} for each fragment
#' @seealso \code{\link[nat]{read.neurons}}, \code{\link{ngl_segments}}
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
#' }
read_segments <- function(x, voxdims=c(32,32,40), ...) {
  x=ngl_segments(x)
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

#' @rdname read_segments
#' @description \code{read_segments2} is a reworked version of
#'   \code{read_segments} that reads skeletons straight from zip files to
#'   memory.
#' @details I would recommend \code{read_segments2} at this point.
#'   \code{read_segments} has the potential benefit of caching SWC files on disk
#'   rather than extracting every time. However there is a large slowdown on
#'   many filesystems as the number of extracted files enters the thousands -
#'   something that I have hit a few times. Furthermore \code{read_segments2}
#'   makes it easier to select fragment files \emph{before} extracting them.
#' @param minfilesize The uncompressed size of the swc file must be >= this. A
#'   cheap way to insist that we have >1 point.
#' @export
#' @importFrom nat data.frame<-
read_segments2 <- function(x, voxdims=c(32,32,40), minfilesize=80, ...) {
  x=ngl_segments(x)
  zl=lapply(x, skelsforsegment, returndetails=TRUE)
  zdf=dplyr::bind_rows(zl)
  zdf=zdf[zdf$uncompressed_size>=minfilesize,]
  zdf=cbind(zdf, swc2segmentid(zdf$filename, include.fragment = T))

  rownames(zdf)=tools::file_path_sans_ext(zdf$filename)
  ff=zdf$filename
  names(ff)=tools::file_path_sans_ext(ff)
  res=nat::nlapply(ff, read.neuron.from.zip, voxdims=voxdims, ...)
  data.frame(res)=zdf
  res
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

read.neuron.from.zip <- function(file, voxdims=NULL) {
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
  nat::as.neuron(res, InputFileName=file)
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
#' @export
#' @rdname read_topn
find_topn <- function(zipfiles, n=1) {
  if(is.numeric(zipfiles) || !all(file.exists(zipfiles))) {
    zipfiles=zip_path(zipfiles)
  }
  # forget on way in to keep cache reasonable but to allow read to use later
  forget(zip_list_m)
  process1 <- function(zipfile){
    zl=zip_list_m(zipfile)
    pb$tick()
    zl$segment=swc2segmentid(zl$filename)
    zl %>%
      group_by(segment) %>%
      summarise(total_size=sum(uncompressed_size), nfragments=n()) %>%
      top_n(n, total_size) %>%
      arrange(desc(total_size)) %>%
      mutate(zipfile=zipfile, seq=1:n())
  }

  pb <- progress::progress_bar$new(total = length(zipfiles), show_after=0.5,
    format = "  find_topn [:bar] :percent eta: :eta")

  ll=lapply(zipfiles, process1)
  dplyr::bind_rows(ll)
}
