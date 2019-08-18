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

  invisible()
}

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
