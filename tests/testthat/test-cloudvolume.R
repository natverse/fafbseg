test_that("secrets work", {
  skip_if_not_installed('mockery')
  skip("Skipping due to mockery stub issues")
  # see https://github.com/r-lib/mockery/issues/30
  td=tempfile()
  on.exit(unlink(td, recursive = T))
  mockery::stub(set_chunkedgraph_token, "cv_secretdir", td, depth = 3)
  mockery::stub(chunkedgraph_token, "cv_secretdir", td, depth = 3)
  mockery::stub(chunkedgraph_token, "Sys.getenv", "", depth=3)

  expect_error(chunkedgraph_token(cached = FALSE), regexp = "Unable to find")

  faketoken="2f88e16c4f21bfcb290b2a8288c05bd0"
  expect_error(set_chunkedgraph_token("1234"))
  expect_silent(set_chunkedgraph_token(faketoken))

  expect_equal(chunkedgraph_token(cached = FALSE), faketoken)

})
