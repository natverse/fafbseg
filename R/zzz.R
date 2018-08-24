.onLoad <- function(libname, pkgname) {
  op.fafbseg=list(
    fafbseg.sampleurl = "https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%7B%22fafb_v14_clahe%22:%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb_v14_clahe%22,%22type%22:%22image%22%7D,%22fafb_v14_16nm_v00c_split3xfill2%22:%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2%22,%22type%22:%22segmentation%22%7D%7D,%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:[8,8,40],%22voxelCoordinates%22:[53323.1017,21251.8736,3282]%7D%7D,%22zoomFactor%22:8%7D,%22showAxisLines%22:false,%22perspectiveOrientation%22:[0.0214,0.4873,-0.505,-0.7121],%22perspectiveZoom%22:1024,%22showSlices%22:false%7D"
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

  invisible()
}


.onUnload <- function(libpath) {
  td <- temproot()
  if(file.exists(td)) {
    if(interactive())
      message("fafbseg: removing cached skeletons")
    unlink(td, recursive = TRUE)
  }
}
