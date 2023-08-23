test_that("flywire nuclei works", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if_not_installed('reticulate')
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")

  # make sure this an up to date root id
  rid=flywire_rootid('80999991094644060')
  expect(nrow(n1 <- flywire_nuclei(rid, rawcoords = T)), 1L)
  expect_equal(flywire_nearest_nuclei(n1$pt_position, rawcoords = T)[1:ncol(n1)], n1)
})
