test_that("multiplication works", {
  skip_if_not_installed('fastmap')
  v64 <- vcache64('rhubarb-test')
  expect_silent(vcache_mset(v64, '1', bit64::as.integer64(1L)))
  expect_error(vcache_mset(v64, '1', 1L), regexp = "class mismatch")
  expect_equal(vcache_mget(v64, '1'), bit64::as.integer64(1L))
  expect_equal(vcache_mget(v64, '-1'), bit64::as.integer64(0L))
})

test_that("vcache_mset handles large integer64 vectors", {
  skip_if_not_installed('fastmap')
  # as.list.integer64() overflows the protection stack past ~R_PPStackSize
  # (bit64 <=4.8.2, r-lib/bit64#346); vcache_mset chunks to avoid this.
  n <- 60000L
  v64 <- vcache64('rhubarb-big-test')
  keys <- as.character(seq_len(n))
  vals <- bit64::as.integer64(seq_len(n))
  expect_silent(vcache_mset(v64, keys, vals))
  expect_equal(vcache_mget(v64, keys), vals)
})
