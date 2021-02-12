.onLoad <- function(libname, pkgname) {
  op.fafbseg=choose_segmentation('flywire', set=FALSE)
  # set a default location for sqlite databases if user has not specified their
  # own
  op.fafbseg=c(op.fafbseg, list('fafbseg.sqlitepath'="~/projects/JanFunke/"))

  op.fafbseg=c(op.fafbseg, list('fafbseg.cachedir'=rappdirs::user_data_dir('R/fafbseg')))

  op<-options()
  toset <- !(names(op.fafbseg) %in% names(op))
  if(any(toset)) options(op.fafbseg[toset])

  # make FAFB<->FlyWire bridging registrations available
  register_fafb_flywire()
  # make FANC4<->FANC3 bridging registrations available
  register_fanc3to4()

  invisible()
}

# memoised so that we can change cache dir during a session
flywire_leaves_cache <- memoise::memoise(function(cachedir=getOption("fafbseg.cachedir"), hybrid=FALSE) {
  check_package_available('cachem')
  d <- cachem::cache_disk(max_size = 1.5 * 1024^3, dir = cachedir)
  if(isTRUE(hybrid)) {
    # unclear that mem cache gives any useful benefit given compression cycle
    m <- cachem::cache_mem(max_size = 200 * 1024^2)
    cl <- cachem::cache_layered(m, d)
    cl
  } else d
})

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
