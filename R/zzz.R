.onLoad <- function(libname, pkgname) {
  op.fafbseg=list(
    fafbseg.sampleurl = "https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%7B%22fafb_v14_clahe%22:%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb_v14_clahe%22,%22type%22:%22image%22%7D,%22fafb_v14_16nm_v00c_split3xfill2%22:%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2%22,%22type%22:%22segmentation%22%7D%7D,%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:[8,8,40],%22voxelCoordinates%22:[53323.1017,21251.8736,3282]%7D%7D,%22zoomFactor%22:8%7D,%22showAxisLines%22:false,%22perspectiveOrientation%22:[0.0214,0.4873,-0.505,-0.7121],%22perspectiveZoom%22:1024,%22showSlices%22:false%7D",
    fafbseg.brainmaps.volume="772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2",
    fafbseg.brainmaps.meshName="mcws_quad1e6",
    fafbseg.skeletonuri="brainmaps://772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2_flatreseg2_skeletons32/teasar512_nnconn165_mc10000_prune10_thresh1000_sparse250"

  )

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
# althoug querying an option is very fast - even compared with getting the
# results of a memoised function
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
