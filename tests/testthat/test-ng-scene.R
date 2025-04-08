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
  # text for voxel dimensions while we're at it ...
  expect_equivalent(voxdims(fu), c(16,16,40))
  # just reuse the existing matrix so we can test for (almost) equality
  m2=fu$layers$fly_v31_mirror$source$transform$matrix
  m2=rbind(m2,c(0,0,0,1))
  expect_is(sc3 <- xform(fu, reg = m2, layers = c(fly_v31_mirror="fly_v31")),
               'ngscene')
  expect_equal(sc3$layers, fu$layers, tolerance = 1e-3)

  # from https://tinyurl.com/flywire783hb
  # has 2 sources
  fw783_layer=list(
    type = "segmentation",
    source = list(
      list(url = "precomputed://gs://flywire_v141_m783",
         subsources = list(default = TRUE, mesh = TRUE),
         enableDefaultSubsources = FALSE),
      "precomputed://https://flyem.mrc-lmb.cam.ac.uk/flyconnectome/dynann/flytable-info-783"),
    tab = "source",
    name = "fw783")
  sc3$layers$fw783=fw783_layer
  expect_is(sc4 <- xform(sc3, reg = m2, layers = c(fw783_mirror="fw783")),
            'ngscene')
  expect_equal(sc4$layers$fw783$source[[1]]$url,
               sc4$layers$fw783_mirror$source[[1]]$url)

  xform_baseline=list(
    matrix = structure(c(-0.9923, -0.0765, 0.167, -0.0451,
                         0.9961, 0.0059, 0.0652, 0.0048,
                         0.9915, 65672.5875, 2454.3162, -2227.4943), dim = 3:4),
    outputDimensions = list(x = c("1.6e-08", "m"),
                            y = c("1.6e-08", "m"),
                            z = c("4e-08", "m")))
  expect_equal(sc4$layers$fw783_mirror$source[[1]]$transform, xform_baseline)

})
