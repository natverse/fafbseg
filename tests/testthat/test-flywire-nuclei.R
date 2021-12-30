test_that("flywire nuclei works", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if_not_installed('reticulate')
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")

  expect(nrow(n1 <- flywire_nuclei("720575940618824027")), 1L)
  expect_equal(flywire_nearest_nuclei(n1$pt_position)[1:ncol(n1)], n1)
})
