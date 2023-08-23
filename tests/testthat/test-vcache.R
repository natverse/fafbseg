test_that("multiplication works", {
  skip_if_not_installed('fastmap')
  v64 <- vcache64('rhubarb-test')
  expect_silent(vcache_mset(v64, '1', bit64::as.integer64(1L)))
  expect_error(vcache_mset(v64, '1', 1L), regexp = "class mismatch")
  expect_equal(vcache_mget(v64, '1'), bit64::as.integer64(1L))
  expect_equal(vcache_mget(v64, '-1'), bit64::as.integer64(0L))
})
