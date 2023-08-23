library(testthat)
library(fafbseg)

# try to set token from environment variable on travis
if(identical(Sys.getenv("TRAVIS"), "true")) {
  token <- Sys.getenv('CHUNKEDGRAPH_SECRET')
  if(nzchar(token)) {
    message("Found token of length ", nchar(token), " in CHUNKEDGRAPH_SECRET")
    p <- flywire_set_token(token)
    message("written to: ", p)
  }
}

if(nzchar(Sys.getenv("FLYWIRE_PRINCIPLES")))
  download_flywire_release_data()

op <- options('fafbseg.cachedir'=tempfile('fafbseg-tempcache'))
test_check("fafbseg")
options(op)
