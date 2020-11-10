test_that("dr_fafbseg works", {
  skip('Do not yet have dr_fafbseg() tests configured')
  expect_output(dr_fafbseg(), "java", ignore.case=TRUE)
})

test_that("pyids2bit64 works", {
  skip_if_not_installed('bit64')
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  reticulate::py_run_string('import numpy as np')

  sids=c("720575940625861628","720575940621611957")
  expr=sprintf("np.array([%s])", paste(sids, collapse=","))
  pyids=reticulate::py_eval(expr, convert = F)

  expect_equal(pyids2bit64(pyids, as_character = T), sids)
})
