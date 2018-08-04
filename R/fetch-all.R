#' Fetch data specified by a set of curl shell comands (e.g. from Chrome)
#'
#' @details You can generate an appropriate set of commands by opening the
#'   Chrome Developer console (View ... Developer ... JavaScript Console),
#'   (re)loading a page of interest, selecting the network tab, selecting a
#'   downloaded object, right clicking and then choosing (Copy ... Copy all as
#'   cURL). You should make sure that you only have one neuron displayed if you
#'   do not want to have to parse the object identifier relationships.
#'
#' @param x Path to a file or character vector of statements (by default it
#'   reads statements from the clipboard)
#' @param outdir Optional output directory (will be created if necessary)
#' @param regex Optional regular expression that curl statements must match
#' @param filename A \code{\link{sprintf}} style format statement that will be
#'   used to name the downloaded files.
#' @param ... Additional arguments to \code{\link{grep}}
#'
#' @return A named character vector containing the matched URLs named by the
#'   downloaded files on disk (invisibly)
#' @export
#' @importFrom progress progress_bar
#' @seealso \code{\link{read_ng_dump}}
#' @examples
#' \dontrun{
#' fetch_all_curl("all_curl.sh", outdir="alldata",
#'   regex="brainmaps.googleapis.com", fixed=TRUE)
#' }
fetch_all_curl <- function(x=clipr::read_clip(), outdir=NULL, regex="^curl",
                           filename="chunk%05d.raw", ...) {
  if(length(x)==1 && file.exists(x)) {
    x=readLines(x)
  }
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
  names(x)=sprintf(filename, seq_along(x))
  attr(x, 'outdir')=outdir
  invisible(x)
}


is.json <- function(x) {
  if(length(x)>1) return(sapply(x, is.json))
  if(file.info(x)$size==0) return(FALSE)
  firstchar=readBin(x,what = raw(), n = 1)
  isTRUE(firstchar==0x7b)
}

#' Read neuroglancer data dump from disk / web
#'
#' @param x Path to directory, zip file, or set of data files
#' @param ... Additional argument passed to \code{\link{read_ng_raw}}
#' @details A neuroglancer data dump consists of a mix of JSON and custom binary
#'   data files, which together define mesh data for (fragments) of neurons
#'   together with associated metadata. See \code{\link{fetch_all_curl}} for how
#'   to prepare such a data dump.
#' @return A list of class \code{ng_raw_list} with additional metadata as
#'   attributes.
#' @export
#' @seealso \code{\link{fetch_all_curl}}, \code{\link{read_ng_raw}}
#'
#' @examples
#' \dontrun{
#' fetch_all_curl("all_curl.sh", outdir="alldata",
#'   regex="brainmaps.googleapis.com", fixed=TRUE)
#' meshdata=read_ng_dump("alldata")
#' m=as.mesh3d(meshdata)
#' shade3d(m, col='red')
#'
#' # can also be a (remote) zip file
#' meshdata=read_ng_dump("https://myfiles.com/myneuron.zip")
#' }
read_ng_dump <- function(x, ...) {
  if(length(x)==1){
    if(grepl("^http[s]{0,1}://", x)) {
      ext=tools::file_ext(x)
      if(!isTRUE(ext=='zip')) {
        message("I am assuming that this URL points to a zip file")
      }
      tf <- tempfile(fileext = '.zip')
      curl::curl_download(x, destfile = tf)
      on.exit(unlink(tf))
      x <- tf
    }
    if(tools::file_ext(x)=='zip' && file_test('-f', x)) {
      zipfile=x
      td <- tempfile('fafbseg-temp')
      dir.create(td)
      on.exit(unlink(td, recursive = TRUE), add=TRUE)
      unzip(zipfile, junkpaths = TRUE, exdir = td)
      x <- td
    }
    if(file_test('-d', x)) {
      owd=setwd(x)
      on.exit(setwd(owd), add = TRUE)
      x=dir()
    }
  }
  isjson=is.json(x)
  jsons=x[isjson]

  # read the actual data
  res=read_ng_raw(x[!isjson], ...)

  if(!length(jsons))
    return(res)

  # read metadata contained in JSON results
  info=sapply(jsons, jsonlite::fromJSON)
  # Let's pull out the list of chunk ids
  ischunkid=sapply(info, function(x) {is.list(x) && length(x)==2 && "fragmentKey" %in% names(x)})
  chunkdf=data.frame(fragmentKey=unlist(sapply(info[ischunkid], "[[", "fragmentKey"), use.names = F),
               supervoxelId=unlist(sapply(info[ischunkid], "[[", "supervoxelId"), use.names = F))

  lres=length(res)
  nchunks=nrow(chunkdf)
  if(!isTRUE(all.equal(lres, nchunks))) {
    warning("mismatch between number of data chunks fetched (",
            lres,
            ") and number expected (", nchunks,")")
  }
  attr(res, 'ids')=chunkdf
  attr(res, 'info')=info[!ischunkid]
  res
}
