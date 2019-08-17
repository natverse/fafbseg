.onLoad <- function(libname, pkgname) {
  op.fafbseg=choose_segmentation('20190805', set=FALSE)

  op<-options()
  toset <- !(names(op.fafbseg) %in% names(op))
  if(any(toset)) options(op.fafbseg[toset])

  # make baseurl option from sampleurl if unset
  bu <- getOption("fafbseg.baseurl")
  if(is.null(bu)) {
    su=getOption('fafbseg.sampleurl')
    options(fafbseg.baseurl=sub("^([^#]+)/#!.*","\\1",su))
  }

  zipdir=getOption("fafbseg.skelziproot")

  # if zip file divisor is unset, check zip files
  if(is.null(getOption("fafbseg.zipdivisor"))) {
    if(!is.null(zipdir) && !is.null(divisor <- find_zip_divisor(zipdir))) {
      options(fafbseg.zipdivisor=divisor)
      packageStartupMessage(sprintf('fafbseg: setting: options(fafbseg.zipdivisor=%f)', divisor))
    }
  }
  invisible()
}

find_zip_divisor <- function(zipdir) {
  zips = dir(zipdir, pattern = '\\.zip$', full.names = T)
  if (length(zips)) {
    # if there are some zip files, then list one and figure out the
    # divisor that converts segment ids to zip files
    zip1 = zips[1]
    zl = zip_list(zip1)
    swc = zl[['filename']][1]
    signif(swc2segmentid(swc) / zip2segmentstem(zip1), digits = 1)
  } else NULL
}

# in future may use memoised version of this to avoid explictly
# setting the fafbseg.zipdivisor option
# although querying an option is very fast - even compared with getting the
# results of a memoised function (~)
# ofind_zip_divisor <- memoise::memoise(ofind_zip_divisor)


.onAttach <- function(libname, pkgname) {
  zipdir=getOption("fafbseg.skelziproot")
  if(is.null(zipdir)) {
    packageStartupMessage('fafbseg: set:\n  options(fafbseg.skelziproot="/path/to/zips")\n',
    'so I know where to find zip files containing skeletons')
  }
  invisible()
}

.onUnload <- function(libpath) {
  # check if temproot was ever called
  called <- memoise::has_cache(temproot)()
  if(called && length(dir(temproot(), include.dirs = T))) {
    if(interactive())
      message("fafbseg: removing cached skeletons")
    unlink(temproot(), recursive = TRUE)
  }
  invisible()
}
