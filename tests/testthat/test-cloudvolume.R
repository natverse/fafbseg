test_that("secrets work", {
  skip_if_not_installed('mockery')
  skip("Skipping due to mockery stub issues")
  # see https://github.com/r-lib/mockery/issues/30
  td=tempfile()
  on.exit(unlink(td, recursive = T))
  mockery::stub(flywire_set_token, "cv_secretdir", td, depth = 3)
  mockery::stub(chunkedgraph_token, "cv_secretdir", td, depth = 3)
  mockery::stub(chunkedgraph_token, "Sys.getenv", "", depth=3)

  expect_error(chunkedgraph_token(cached = FALSE), regexp = "Unable to find")

  faketoken="2f88e16c4f21bfcb290b2a8288c05bd0"
  expect_error(flywire_set_token("1234"))
  expect_silent(flywire_set_token(faketoken))

  expect_equal(chunkedgraph_token(cached = FALSE), faketoken)

})


test_that("reading/saving meshes works", {
  token=try(chunkedgraph_token(), silent = TRUE)
  cvavailable=try(check_cloudvolume_reticulate(), silent = T)
  skip_if(inherits(token, "try-error") || inherits(cvavailable, "try-error"),
          "Skipping live flywire tests")
  td=tempfile()
  kcid="720575940623755722"
  expect_match(save_cloudvolume_meshes(kcid, savedir = td, format='ply'),
               'ply$')
  expect_s3_class(read_cloudvolume_meshes(kcid, format='ply', savedir=td), 'neuronlist')
})
