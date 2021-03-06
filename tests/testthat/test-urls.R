context("test-urls")

ngurl="https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:[%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb14_4nm_split4x_fill2:ffnreseg_whitened_16nm_32nm_4nm%22,%22type%22:%22segmentation%22,%22segments%22:[%220%22,%223140809165%22,%224193866734%22,%224256801664%22,%224314929385%22,%224373082358%22,%224610419193%22,%224672751347%22],%22name%22:%22agglo16_32_4_gt3um%22%7D],%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:[8,8,40],%22voxelCoordinates%22:[78997,25574,3600]%7D%7D,%22zoomFactor%22:13.1%7D,%22showAxisLines%22:false,%22showDefaultAnnotations%22:false,%22perspectiveOrientation%22:[0.08,0.22,-0.15,-0.95],%22perspectiveZoom%22:2382,%22showSlices%22:false,%22layout%22:%223d%22%7D"

test_that("decode scene works", {
  expect_error(ngl_decode_scene('https'))
  expect_error(ngl_decode_scene('{'))

  # url="https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb14_4nm_split4x_fill2:ffnreseg_whitened_16nm_32nm_4nm%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%220%22%2C%223140809165%22%2C%224193866734%22%2C%224256801664%22%2C%224314929385%22%2C%224373082358%22%2C%224610419193%22%2C%224672751347%22%5D%2C%22name%22:%22agglo16_32_4_gt3um%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B8%2C8%2C40%5D%2C%22voxelCoordinates%22:%5B78997.46875%2C25574.220703125%2C3600.365966796875%5D%7D%7D%2C%22zoomFactor%22:13.123994088552514%7D%2C%22showAxisLines%22:false%2C%22showDefaultAnnotations%22:false%2C%22perspectiveOrientation%22:%5B0.08349629491567612%2C0.2258872538805008%2C-0.15645836293697357%2C-0.9578747749328613%5D%2C%22perspectiveZoom%22:2382.282469476382%2C%22showSlices%22:false%2C%22layout%22:%223d%22%7D"
  # after url decoding, reducing precision and re-encoding

  expect_is(sc <- ngl_decode_scene(ngurl), "ngscene")
  expect_known_value(sc, file = "testdata/ngscene.rds")
  with_segmentation("20190805", expect_equal(ngl_encode_url(sc), ngurl))
  # expect_is(json <- ngl_decode_scene(ngurl, return.json = TRUE), "character")
  # writeLines(json, con='tests/testthat/testdata/ngurl.json')
  expect_equal(ngl_decode_scene(ngurl, return.json = TRUE),
               readLines('testdata/ngurl.json'))

  # check we can cope with a scene as input as a convenience
  expect_equal(ngl_decode_scene(sc), sc)
})

test_that("we can work round toJSON array issue",{
  # This URL only refers to one segment
  # toJSON has a problem in
  singleton_url="https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb14_4nm_split4x_fill2:ffnreseg_whitened_16nm_32nm_4nm%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%223140809165%22%5D%2C%22name%22:%22agglo16_32_4_gt3um%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B8%2C8%2C40%5D%2C%22voxelCoordinates%22:%5B78997%2C25574%2C3600%5D%7D%7D%2C%22zoomFactor%22:13.1%7D%2C%22showAxisLines%22:false%2C%22showDefaultAnnotations%22:false%2C%22perspectiveOrientation%22:%5B0.08082044869661331%2C0.22225624322891235%2C-0.15153835713863373%2C-0.9597428441047668%5D%2C%22perspectiveZoom%22:2382%2C%22showSlices%22:false%2C%22layout%22:%223d%22%7D"
  sc2=ngl_decode_scene(singleton_url)
  sc2.rt=ngl_decode_scene(ngl_encode_url(sc2), return.json = TRUE)
  expect_match(sc2.rt, '"segments":["3140809165"]', fixed = T)

  f=system.file("flywire-annotations.json" , package = 'fafbseg')
  expect_is(u <- ngl_encode_url(f), 'character')
  expect_equal(ngl_encode_url(ngl_decode_scene(u)), u)
  # round trip test for a singleton annotation
  expect_is(u <- ngl_encode_url(test_path("testdata/flywire-elipse.json")), 'character')
  expect_equal(ngl_encode_url(ngl_decode_scene(u)), u)
  # alternative way to convert scene to URL
  expect_equal(as.character(ngl_decode_scene(u)), u)
})


test_that('we can make a neuroglancer URL', {
  catmaid_url <- "https://fafb.catmaid.virtualflybrain.org/?pid=2&zp=131280&yp=170014.98879622458&xp=426584.81386896875&tool=navigator&sid0=2&s0=-1"
  expect_match(url <- with_segmentation("flywire31", open_fafb_ngl(catmaid_url, open = F)),
               "flywire")
  expect_equal(xyzmatrix(ngl_decode_scene(url)),
               xyzmatrix(catmaid::catmaid_parse_url(catmaid_url)))
})


test_that('we can print scene/layer summaries', {
  releases = eval(formals(choose_segmentation)[['release']])
  for (r in releases) {
    u=getOption("fafbseg.sampleurl")
    sc=ngl_decode_scene(u)
    expect_output(with_segmentation(r, print(sc)),
    "neuroglancer scene with .* layers")

    expect_is(ngl_layers(sc, type=='segmentation'), 'nglayers')
    expect_is(ngl_layers(sc, visible & nsegs>0), 'nglayers')
  }
})

test_that('we can manipulate layers using +/-', {
  f=system.file("flywire-annotations.json" , package = 'fafbseg')
  sc=ngl_decode_scene(f)
  expect_is(sc2 <- sc-c("annotation", "jfrc_mesh_test"), 'ngscene')
  expect_equal(length(ngl_layers(sc2)), 2L)
  expect_is(sc3 <- sc2+ngl_layers(sc)["jfrc_mesh_test"], 'ngscene')
  expect_equal(length(ngl_layers(sc3)), 3L)
})


test_that('we can extract annotations', {
  f=system.file("flywire-annotations.json" , package = 'fafbseg')
  sc=ngl_decode_scene(f)
  expect_is(ann <- ngl_layers(sc, 'annotation'), 'nglayers')
  expect_equal(ngl_layers(sc, type=='annotation'), ann)
})

test_that('we can colour a scene object', {
  f=system.file("flywire-annotations.json" , package = 'fafbseg')
  sc=ngl_decode_scene(f)
  expect_is(sc2 <- ngl_add_colours(sc, c("720575940616120581"="red")), "ngscene")
  expect_equal(sc2$layers$`Production-segmentation_with_graph`$segmentColors,
               list(`720575940616120581` = col2hex("red")))
  expect_equal(ngl_add_colours(sc, c("red")), sc2)
  expect_is(sc3 <- ngl_add_colours(sc2, c("1"="red", "720575940616120581"="green")), "ngscene")
  expect_equal(sc3$layers$`Production-segmentation_with_graph`$segmentColors,
               list('1' = col2hex('red'), `720575940616120581` = col2hex("green")))
  expect_error(ngl_add_colours(sc, c("-1" = "red")), "invalid ids")
  expect_error(ngl_add_colours(sc, c("2" = "rhubarb")), "invalid color name 'rhubarb'")

  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")

  u1="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5695474417795072"
  u2="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5198787572137984"
  u3="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5673953041317888"
  # sequentially build up a data.frame with the colour information
  # note that col will be recycled to the same length as the number of segments
  colourdf=data.frame(ids=ngl_segments(u1), col='red')
  colourdf=rbind(colourdf, data.frame(ids=ngl_segments(u2), col='green'))
  colourdf=rbind(colourdf, data.frame(ids=ngl_segments(u3), col='blue'))
  colourdf$col=factor(colourdf$col)
  # apply that to the first URL
  expect_is(sc <- ngl_add_colours(u1, colourdf), 'ngscene')
  expect_equal(ngl_segments(sc), colourdf$ids)
})


test_that('we can get blank scene urls', {
  expect_s3_class(sc <- ngl_blank_scene(), 'ngscene')
  kcid="720575940623755722"
  expect_equal(ngl_segments(sc+kcid), kcid)
  expect_match(ngl_segmentation(ngl_blank_scene("202004")+1), "ffn1-20200412")
  expect_match(ngl_segmentation(ngl_blank_scene("202004", return.url = T)),
               "ffn1-20200412")
  expect_match(ngl_blank_scene("202004", return.url = T),
               baseurl_from_url(
                 with_segmentation("202004", getOption("fafbseg.sampleurl"))))

})
