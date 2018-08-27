context("test-ids")

test_that("simple ids", {
  expect_equal(segmentid2zip(10001654273), "100016.zip")

  baseline = structure(
    c(10001654273, 10001654273, 10001654273, 0, 1, 2),
    .Dim = 3:2,
    .Dimnames = list(NULL, c("segment", "fragment"))
  )

  expect_equal(swc2segmentid(sprintf("10001654273.%d.swc", 0:2), include.fragment=TRUE),
               baseline)

  expect_equal(segmentid2zip(swc2segmentid("10001654273.1.swc")), "100016.zip")
})
