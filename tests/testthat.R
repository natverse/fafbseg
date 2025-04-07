library(testthat)
library(fafbseg)

if(nzchar(Sys.getenv("FLYWIRE_PRINCIPLES")))
  download_flywire_release_data()

op <- options('fafbseg.cachedir'=tempfile('fafbseg-tempcache'))
test_check("fafbseg")
options(op)
