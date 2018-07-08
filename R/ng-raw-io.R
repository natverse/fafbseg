read_ng_raw_header <- function(con) {
  if (is.character(con)) {
    con = file(con, open = 'rb')
    on.exit(close(con))
  }
  h=list()
  h$header=readBin(con, what=raw(), n = 8)
  # h$something=readBin(con, what=integer(), n = 1, size=4)
  h$filenamelen=readBin(con, what=integer(), n = 1, size=4)
  if(h$filenamelen>255)
    stop("funny filenamelen value:", h$filenamelen)
  h$blank=readBin(con, what=integer(), n = 1, size=4)
  h$name=readChar(con, nchars = h$filenamelen, useBytes = T)
  h$lens=readBin(con, what=integer(), n = 2, size=8)
  h
}

read_ng_raw_chunk <- function(con, read_data=TRUE, Verbose=FALSE) {
  h=read_ng_raw_header(con)
  if(Verbose) {
    message("Data lengths: ", paste(h$lens, collapse=","))
    message("Data start position:", seek(con))
  }
  # read coords as 4 byte floats
  res=list(h=h)
  if(read_data) {
    res$coords = matrix(
      readBin(con, what = numeric(), size = 4, n = h$lens[1] * 3),
      ncol = 3, byrow = T)
    colnames(res$coords)=c("X","Y","Z")
    res$indices=matrix(
      readBin(con, what=integer(), size=4, n=h$lens[2]*3),
      ncol=3, byrow = T)
  } else {
    seek(con, sum(h$lens * 3 * 4), origin="current")
  }
  class(res)='ng_raw'
  res
}

#' Read raw neuroglancer data - currently only supports mesh format
#'
#' @param x Path to one or more files
#' @param read_data Whether to read the data (default when \code{TRUE}) or just
#'   the header
#' @param Verbose Whether to print some status messages (default \code{FALSE})
#'
#' @return An object of class 'ng_raw_list' containing one or more chunks of
#'   data of class 'ng_raw'. When \code{x} contains multiple files, all the
#'   chunks are merged into a single list.
#' @export
#' @references See \url{https://github.com/google/neuroglancer}
#'
#' @examples
#' \dontrun{
#' res <- read_ng_raw("meshdata/chunk00789.raw")
#' resh <- read_ng_raw("meshdata/chunk00789.raw", read_data=FALSE)
#' resl <- read_ng_raw(dir("meshdata", full.names = TRUE)
#' }
read_ng_raw <- function(x, read_data=TRUE, Verbose=FALSE) {
  if(length(x)>1) {
    pb <- progress_bar$new(
    total = length(x), show_after=0.5,
    format = "  reading [:bar] :percent eta: :eta")

    resl <- lapply(x, function(x) {
      t=try(read_ng_raw(x, read_data=read_data, Verbose=Verbose), silent = TRUE)
      pb$tick()
      if(inherits(t, 'try-error')) NULL else t
      })
    res=unlist(resl, recursive = FALSE, use.names = TRUE)
    class(res) <- 'ng_raw_list'
    return(res)
  }
  con=file(x, open = 'rb')
  on.exit(close(con))
  fsize=file.size(x)
  res=list()
  while(seek(con) < fsize){
    thisres=read_ng_raw_chunk(con, read_data = read_data, Verbose=Verbose)
    res[[thisres$h$name]]=thisres
  }
  class(res)='ng_raw_list'
  res
}


#' @export
as.data.frame.ng_raw_list <- function(x, ...) {
  readh <- function(x) {
    ints <- readBin(x$h$header, what='int', endian = 'big',n=2)
    chars <- as.character(as.hexmode(ints))
    paste(chars,collapse = "")
  }
  hexh=sapply(x, readh)
  name=sapply(x, function(x) x$h$name)
  nvertices=sapply(x, function(x) x$h$lens[1])
  nindices=sapply(x, function(x) x$h$lens[2])
  data.frame(name,header=hexh, nvertices, nindices, stringsAsFactors = FALSE)
}
