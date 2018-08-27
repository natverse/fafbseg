context("test-fetch-all.R")

test_that("read_ng_dump works", {
  expect_is(d <- read_ng_dump('testdata/read_ng_dump/'), 'ng_raw_list')
  expect_length(d, 3L)
  expect_named(d, c("000000000003500b", "0000000000035042", "0000000000035008"))
})
