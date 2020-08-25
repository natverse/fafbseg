test_that("multiplication works", {

  locs=matrix(c(29858,109329,1878,
                32918,110461,2174),
              ncol=3, byrow = T)
  baseline=structure(c(29858, 32918, 109329, 110461, 1878, 2174),
                     .Dim = 2:3, .Dimnames = list(NULL, c("X", "Y", "Z")))

  expect_equal(xform_brain(locs, reference = "FANC3", sample='FANC4'),
               baseline)
})
