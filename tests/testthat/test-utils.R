test_that("dr_fafbseg works", {
  expect_output(dr_fafbseg(), "java", ignore.case=TRUE)
})

test_that("pyids2bit64 works", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  reticulate::py_run_string('import numpy as np')

  sids=c("720575940625861628","720575940621611957")
  sids64=bit64::as.integer64(c("720575940625861628","720575940621611957"))
  expr=sprintf("np.array([%s])", paste(sids, collapse=","))
  pyids=reticulate::py_eval(expr, convert = F)

  expect_equal(pyids2bit64(pyids, as_character = T), sids)
  expect_equal(pyids2bit64(pyids, as_character = F), sids64)
  expect_equal(pyids2bit64(pyids[0], as_character = T), sids[1])
  expect_equal(pyids2bit64(pyids[1], as_character = T), sids[2])
  expect_equal(pyids2bit64(rids2pyint(sids, numpyarray = T), as_character = F),
               sids64)
  expect_equal(pyids2bit64(rids2pyint(sids64, numpyarray = T), as_character = F),
               sids64)
  expect_equal(pyids2bit64(rids2pyint(sids64[1], numpyarray = T), as_character = F),
               sids64[1])
  expect_equal(pyids2bit64(reticulate::py_eval("[9223372036854775807]", convert = F)),
                           "9223372036854775807")
  expect_error(pyids2bit64(
    reticulate::py_eval("np.array([9223372036854775808])", convert = F)),
    "int64 overflow")
})

test_that("pandas2df in-memory conversion preserves int64 columns", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  skip_if_not(reticulate::py_module_available("numpy"))
  skip_if_not(reticulate::py_module_available("pandas"))

  np <- reticulate::import("numpy", convert = FALSE)
  pd <- reticulate::import("pandas", convert = FALSE)
  sids <- c("720575940625861628", "720575940621611957")
  df <- pd$DataFrame(list(
    id = np$array(sids, dtype = "int64"),
    uid = np$array(c("123", "456"), dtype = "uint64"),
    label = c("a", "b"),
    score = c(1.5, 2.5),
    ok = c(TRUE, FALSE)
  ))

  out <- pandas2df(df, method = "inmem")
  expect_s3_class(out, "tbl_df")
  expect_true(bit64::is.integer64(out$id))
  expect_true(bit64::is.integer64(out$uid))
  expect_equal(as.character(out$id), sids)
  expect_equal(as.character(out$uid), c("123", "456"))
  expect_identical(out$label, c("a", "b"))
  expect_equal(out$score, c(1.5, 2.5))
  expect_identical(out$ok, c(TRUE, FALSE))
})

test_that("pandas2df in-memory conversion reads pandas Series explicitly", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  skip_if_not(reticulate::py_module_available("numpy"))
  skip_if_not(reticulate::py_module_available("pandas"))

  np <- reticulate::import("numpy", convert = FALSE)
  pd <- reticulate::import("pandas", convert = FALSE)
  sids <- c("720575940625861628", "720575940621611957")
  df <- pd$DataFrame(list(
    id = np$array(sids, dtype = "int64"),
    pt_position = list(c(1L, 2L, 3L), c(4L, 5L, 6L)),
    label = c("a", "b")
  ))
  series <- reticulate::py_get_item(df, "id")
  expect_s3_class(series, "pandas.core.series.Series")
  expect_equal(as.character(series$dtype), "int64")

  out <- pandas2df(df, method = "inmem")
  expect_true(bit64::is.integer64(out$id))
  expect_equal(as.character(out$id), sids)
  expect_identical(out$pt_position, list(1:3, 4:6))
  expect_identical(out$label, c("a", "b"))
})

test_that("pandas2df in-memory conversion patches object ids and datetimes", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  skip_if_not(reticulate::py_module_available("pandas"))

  reticulate::py_run_string("
import pandas as pd
pdf_obj_ids = pd.DataFrame({
    'id': [6507536],
    'created': [pd.Timestamp('2021-06-23 19:55:36')],
    'pt_root_id': [720575940631797753],
    'pt_supervoxel_id': [80999991094644060],
    'pt_position': [[1, 2, 3]]
})
pdf_obj_ids['pt_root_id'] = pdf_obj_ids['pt_root_id'].astype('object')
pdf_obj_ids['pt_supervoxel_id'] = pdf_obj_ids['pt_supervoxel_id'].astype('object')
")
  df <- reticulate::py_eval("pdf_obj_ids", convert = FALSE)

  out <- pandas2df(df, method = "inmem")
  expect_equal(out$id, 6507536)
  expect_s3_class(out$created, "POSIXct")
  expect_true(bit64::is.integer64(out$pt_root_id))
  expect_true(bit64::is.integer64(out$pt_supervoxel_id))
  expect_equal(as.character(out$pt_root_id), "720575940631797753")
  expect_equal(as.character(out$pt_supervoxel_id), "80999991094644060")
  expect_identical(out$pt_position, list(1:3))
})

test_that("tabify_coords works", {
  m=matrix(1:6, ncol=3, byrow = T)
  expect_equal(tabify_coords(m, FUN=I), c("1\t2\t3", "4\t5\t6"))
})

test_that('module_version works',{
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  expect_true(is.na(module_version('rhubarb')))
})

test_that("internet_ok works", {
  skip_if_offline()
  expect_true(internet_ok())
})

test_that("managed Python request helpers distinguish the reticulate sentinel", {
  withr::local_envvar(c(RETICULATE_PYTHON = NA))
  expect_false(fafbseg:::managed_python_requested())
  expect_false(fafbseg:::ownpythonrequested())

  withr::local_envvar(c(RETICULATE_PYTHON = "managed"))
  expect_true(fafbseg:::managed_python_requested())
  expect_false(fafbseg:::ownpythonrequested())
  expect_error(fafbseg:::checkownpython(TRUE), "non-standard Python")

  withr::local_envvar(c(RETICULATE_PYTHON = "/opt/example/bin/python"))
  expect_false(fafbseg:::managed_python_requested())
  expect_true(fafbseg:::ownpythonrequested())
  expect_error(fafbseg:::checkownpython(TRUE), "non-standard Python")
})

test_that("simple_python validates managed Python setup before installing", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("reticulate")

  expect_error(
    simple_python("none", miniconda = FALSE, managed = TRUE),
    "incompatible"
  )

  withr::local_envvar(c(RETICULATE_PYTHON = "/opt/example/bin/python"))
  expect_error(
    simple_python("none", managed = TRUE),
    "conflicts with `managed=TRUE`"
  )

  withr::local_envvar(c(RETICULATE_PYTHON = NA))
  mockery::stub(simple_python, "reticulate::py_available", TRUE)
  expect_error(
    simple_python("none", managed = TRUE),
    "Python is already active"
  )
})

test_that("simple_python uses py_require in managed mode", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("reticulate")

  required <- character()
  require_mock <- function(packages) {
    required <<- c(required, packages)
    invisible(packages)
  }

  withr::local_envvar(c(RETICULATE_PYTHON = NA))
  mockery::stub(simple_python, "check_reticulate", TRUE)
  mockery::stub(simple_python, "simple_python_base", FALSE)
  mockery::stub(simple_python, "managed_python_require", require_mock)
  mockery::stub(simple_python, "reticulate::py_available", FALSE)
  mockery::stub(simple_python, "reticulate::py_config",
                function() list(python = "mock-managed-python"))

  expect_message(
    simple_python("basic", pkgs = "fafbseg", managed = TRUE),
    "reticulate-managed Python"
  )
  expect_identical(Sys.getenv("RETICULATE_PYTHON"), "managed")
  expect_equal(required, c("cloud-volume", "seatable_api!=2.6.3",
                           "caveclient", "fafbseg"))
})

test_that("simple_python skips optional conda-only packages in managed mode", {
  skip_if_not_installed("mockery")
  skip_if_not_installed("reticulate")

  withr::local_envvar(c(RETICULATE_PYTHON = NA))
  mockery::stub(simple_python, "check_reticulate", TRUE)
  mockery::stub(simple_python, "simple_python_base", FALSE)
  mockery::stub(simple_python, "managed_python_require",
                function(packages) invisible(packages))
  mockery::stub(simple_python, "reticulate::py_available", FALSE)
  mockery::stub(simple_python, "reticulate::py_config",
                function() list(python = "mock-managed-python"))

  expect_warning(
    simple_python("extra", managed = TRUE),
    "Skipping optional pyembree"
  )
})
