context("test-ids")

test_that("simple ids", {
  # temporarily use test zip folder
  op <- options(fafbseg.skelziproot = 'testdata/skeletonzip/')
  on.exit(options(op))

  expect_equal(segmentid2zip(10001654273), "10001.zip")

  baseline = structure(
    c(10001654273, 10001654273, 10001654273, 0, 1, 2),
    .Dim = 3:2,
    .Dimnames = list(NULL, c("segment", "fragment"))
  )
  expect_equal(swc2segmentid(sprintf("10001654273.%d.swc", 0:2), include.fragment=TRUE),
               baseline)

  expect_equal(segmentid2zip(swc2segmentid("10001654273.1.swc")), "10001.zip")

  # check for error when we specify a non-existent folder
  options(fafbseg.skelziproot = tempfile())
  expect_error(segmentid2zip(10001654273))
})

test_that('ngl_segments', {
  baseline=c(10950626347, 10952282491, 13307888342)
  # json file
  expect_equal(ngl_segments("testdata/testscene.json"), baseline)
  # json text
  expect_equal(ngl_segments(readLines("testdata/testscene.json")), baseline)
  # R list
  scene=jsonlite::fromJSON("testdata/testscene.json")
  expect_equal(ngl_segments(scene), baseline)
  expect_equal(ngl_segments(ngl_encode_url(body = scene)), baseline)

  # list with two sets of segments
  expect_error(ngl_segments("testdata/testscene-double.json"))
})

