#' @importFrom utils unzip
#' @importFrom progress progress_bar
extract_zip_files <- function(files, zipfiles=NULL, ...) {
  if(is.character(files)) {
    zipfiles=attr(files, 'zip')
    files=list(files)
  } else {
    if(is.null(zipfiles)) zipfiles=names(zipfiles)
  }

  if(is.null(zipfiles)) {
    stop("Please specify the zipfiles containing the files to extract",
         " as described in ?extract_zip_files")
  }
  names(files)=zipfiles

  nzips=length(zipfiles)
  if(nzips>1 && length(files)!=nzips) {
    stop("files must be a list with one character vector for each zip file!")
  }

  nfiles=length(unlist(files))
  pb <- progress_bar$new(total = nfiles, show_after=0.5,
    format = "  extracting [:bar] :current/:total eta: :eta")

  ff=character()
  for(zipfile in zipfiles) {
    exdir=tempdir4zip(zipfile)
    x=files[[zipfile]]
    pb$tick(length(x))
    xx=file.path(exdir, x)
    new=x[!file.exists(xx)]
    if(length(new))
      unzip(zipfile, new, exdir = exdir, junkpaths=T, overwrite = F, ...)
    ff=c(ff, xx)
  }
  ff
}

# memoised so we only run once
#' @importFrom memoise memoise is.memoised
temproot <- function() {
  td <- tempfile('fafbseg-zips')
  dir.create(td)
  td
}
if(!memoise::is.memoised(temproot)) temproot <- memoise::memoise(temproot)

tempdir4zip <- function(x) {
  zipstem=tools::file_path_sans_ext(basename(x))
  fp=file.path(temproot(), zipstem)
  dir.create(fp, recursive = T)
  fp
}
if(!memoise::is.memoised(tempdir4zip)) tempdir4zip <- memoise::memoise(tempdir4zip)

zip_path <- function(x, root=getOption('fafbseg.skelziproot', NULL), mustWork=NA) {
  if(is.null(root))
    stop("Please set options(fafbseg.skelziproot='path/to/zips') to set location of skeleton zip files!")
  if(isTRUE(all(tools::file_ext(x)==""))) {
    x=paste0(x, '.zip')
  }
  p=file.path(root, x)
  if(isTRUE(!mustWork))
    return(p)
  up=unique(p)
  FUN=ifelse(isTRUE(mustWork), stop, warning)
  fexists <- file.exists(up)
  if(!all(fexists))
    FUN("Cannot find zips: ", paste(basename(up[!fexists]), collapse = " "),
        " at path: ", root)
  p
}

#' @importFrom memoise memoise
#' @importFrom zip zip_list
zip_list_m <- memoise::memoise(zip::zip_list)
