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
