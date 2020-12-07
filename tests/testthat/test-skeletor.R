test_that("skeletor works", {
  skip_if_not_installed('reticulate')
  cv_available=!inherits(try(check_cloudvolume_reticulate(), silent = TRUE), "try-error")
  skip_if_not(cv_available, 'skipping tests requiring cloudvolume')
  skip('skeletor tests not yet functional')
  xyz <- structure(c(375741.718773652, 375624.31567637, 175945.443753022,
                     176156.125824337, 211280, 211160), .Dim = 2:3, .Dimnames = list(
                       NULL, c("X", "Y", "Z")))
  ids <- unique(flywire_xyz2id(xyz))
  neuron <- skeletor(ids)
  expect_is(neuron, 'neuronlist')
})
