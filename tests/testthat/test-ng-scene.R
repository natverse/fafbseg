test_that("multiplication works", {

  mirroreg = structure(
    c(
      -0.993507657550687,
      -0.0789118732716515,
      0.162578427057817,
      0,
      -0.0415891298774246,
      0.998353717776582,
      0.00339175766683097,
      0,
      0.0594215260756958,
      0.0023521675167209,
      0.9951539398219,
      0,
      1054823.85533788,
      41754.6057995975,
      -86025.0536428828,
      1
    ),
    .Dim = c(4L,
             4L)
  )

  expect_is(
    sc2 <- flywire_scene() %>%
    ngl_decode_scene %>%
    xform(mirroreg, layers=c("mirror"="Production-segmentation_with_graph")),
    "ngscene")
  expect_true("mirror" %in% names(sc2$layers))

  expect_equal(xform(sc2, reg = reglist(mirroreg),
                     layers=c("mirror"="Production-segmentation_with_graph")),
               sc2)
  fu=ngl_decode_scene('https://tinyurl.com/rpt4vrh8')
  # just reuse the existing matrix so we can test for (almost) equality
  m2=fu$layers$fly_v31_mirror$source$transform$matrix
  m2=rbind(m2,c(0,0,0,1))
  expect_is(sc3 <- xform(fu, reg = m2, layers = c(fly_v31_mirror="fly_v31")),
               'ngscene')
  expect_equal(sc3$layers, fu$layers, tolerance = 1e-3)
})
