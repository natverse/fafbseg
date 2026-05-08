library(testthat)
library(fafbseg)

mac_ci_trace <- function(...) {
  if(!nzchar(Sys.getenv("CI")) || !identical(Sys.info()[["sysname"]], "Darwin"))
    return(invisible(NULL))
  ts <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  cat(sprintf("[fafbseg-ci %s] %s\n", ts, paste0(..., collapse = "")))
  flush.console()
  invisible(NULL)
}

mac_ci_trace_python <- function() {
  if(!nzchar(Sys.getenv("CI")) || !identical(Sys.info()[["sysname"]], "Darwin"))
    return(invisible(NULL))

  py <- try(reticulate::py_config(), silent = TRUE)
  if(inherits(py, "try-error")) {
    mac_ci_trace("py_config failed: ", conditionMessage(attr(py, "condition")))
  } else {
    mac_ci_trace("python=", py$python)
    mac_ci_trace("pythonhome=", py$pythonhome)
    mac_ci_trace("numpy=", py$numpy)
  }

  secretdir <- try(cv_secretdir(), silent = TRUE)
  if(inherits(secretdir, "try-error")) {
    mac_ci_trace("cv_secretdir failed: ", conditionMessage(attr(secretdir, "condition")))
  } else {
    mac_ci_trace("cv_secretdir=", secretdir)
    mac_ci_trace("cave-secret exists=", file.exists(file.path(secretdir, "cave-secret.json")))
    mac_ci_trace("chunkedgraph-secret exists=", file.exists(file.path(secretdir, "chunkedgraph-secret.json")))
  }

  invisible(NULL)
}

mac_ci_trace("starting tests/testthat.R")
mac_ci_trace_python()

if(nzchar(Sys.getenv("FLYWIRE_PRINCIPLES"))) {
  mac_ci_trace("before download_flywire_release_data")
  download_flywire_release_data()
  mac_ci_trace("after download_flywire_release_data")
}

if(nzchar(Sys.getenv('CI')) && !identical(Sys.info()[["sysname"]], "Darwin")) {
  dr_fafbseg()
}

op <- options('fafbseg.cachedir'=tempfile('fafbseg-tempcache'))
mac_ci_trace("before test_check")
tryCatch(
  test_check("fafbseg"),
  error = function(e) {
    mac_ci_trace("test_check error: ", conditionMessage(e))
    pyerr <- try(reticulate::py_last_error(), silent = TRUE)
    if(!inherits(pyerr, "try-error")) {
      mac_ci_trace("reticulate::py_last_error follows")
      print(pyerr)
    }
    stop(e)
  }
)
mac_ci_trace("after test_check")
options(op)
