.onLoad <- function(libname, pkgname) {
  op.fafbseg=choose_segmentation('20190805', set=FALSE)

  op<-options()
  toset <- !(names(op.fafbseg) %in% names(op))
  if(any(toset)) options(op.fafbseg[toset])

  # make FAFB<->FlyWire bridging registrations available
  register_fafb_flywire()
  # make FANC4<->FANC3 bridging registrations available
  register_fanc3to4()
  invisible()
}

.onAttach <- function(libname, pkgname) {

  packageStartupMessage("Run dr_fafbseg() for a status report on your installation")
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
