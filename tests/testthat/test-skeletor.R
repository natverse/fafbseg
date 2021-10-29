test_that("skeletor works", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")
  skip_on_os("mac")
  skip_if_not(reticulate::py_module_available("skeletor"),
              "Skipping live flywire tests requiring python skeletor module")
  cv_available=!inherits(try(check_cloudvolume_reticulate(), silent = TRUE), "try-error")
  skip_if_not(cv_available, 'skipping tests requiring cloudvolume')

  xyz <- structure(c(713027.626386585, 713042.051094728, 133955.812509071,
                     134126.394370907, 123640, 123480), .Dim = 2:3, .Dimnames = list(
                       NULL, c("X", "Y", "Z")))
  ids <- unique(flywire_xyz2id(xyz))

  neuron <- skeletor(ids, heal = FALSE, method = "wavefront")
  expect_is(neuron, 'neuronlist')
})
