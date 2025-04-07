library(testthat)
library(fafbseg)

if(nzchar(Sys.getenv("FLYWIRE_PRINCIPLES")))
  download_flywire_release_data()

if(nzchar(Sys.getenv('CI')))
  dr_fafbseg()

op <- options('fafbseg.cachedir'=tempfile('fafbseg-tempcache'))
test_check("fafbseg")
options(op)
