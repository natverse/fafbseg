context("test-urls")

ngurl="https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:[%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb14_4nm_split4x_fill2:ffnreseg_whitened_16nm_32nm_4nm%22,%22type%22:%22segmentation%22,%22segments%22:[%220%22,%223140809165%22,%224193866734%22,%224256801664%22,%224314929385%22,%224373082358%22,%224610419193%22,%224672751347%22],%22name%22:%22agglo16_32_4_gt3um%22%7D],%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:[8,8,40],%22voxelCoordinates%22:[78997,25574,3600]%7D%7D,%22zoomFactor%22:13.1%7D,%22showAxisLines%22:false,%22showDefaultAnnotations%22:false,%22perspectiveOrientation%22:[0.08,0.22,-0.15,-0.95],%22perspectiveZoom%22:2382,%22showSlices%22:false,%22layout%22:%223d%22%7D"

test_that("decode scene works", {
  expect_error(ngl_decode_scene('https'))
  expect_error(ngl_decode_scene('{'))

  # url="https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb14_4nm_split4x_fill2:ffnreseg_whitened_16nm_32nm_4nm%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%220%22%2C%223140809165%22%2C%224193866734%22%2C%224256801664%22%2C%224314929385%22%2C%224373082358%22%2C%224610419193%22%2C%224672751347%22%5D%2C%22name%22:%22agglo16_32_4_gt3um%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B8%2C8%2C40%5D%2C%22voxelCoordinates%22:%5B78997.46875%2C25574.220703125%2C3600.365966796875%5D%7D%7D%2C%22zoomFactor%22:13.123994088552514%7D%2C%22showAxisLines%22:false%2C%22showDefaultAnnotations%22:false%2C%22perspectiveOrientation%22:%5B0.08349629491567612%2C0.2258872538805008%2C-0.15645836293697357%2C-0.9578747749328613%5D%2C%22perspectiveZoom%22:2382.282469476382%2C%22showSlices%22:false%2C%22layout%22:%223d%22%7D"
  # after url decoding, reducing precision and re-encoding

  expect_is(sc <- ngl_decode_scene(ngurl), "ngscene")
  expect_known_value(sc, file = "testdata/ngscene.rds")
  expect_equal(ngl_encode_url(sc), ngurl)
  # expect_is(json <- ngl_decode_scene(ngurl, return.json = TRUE), "character")
  # writeLines(json, con='tests/testthat/testdata/ngurl.json')
  expect_equal(ngl_decode_scene(ngurl, return.json = TRUE),
               readLines('testdata/ngurl.json'))
})

test_that("we can work round toJSON array issue",{
  # This URL only refers to one segment
  # toJSON has a problem in
  singleton_url="https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb14_4nm_split4x_fill2:ffnreseg_whitened_16nm_32nm_4nm%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%223140809165%22%5D%2C%22name%22:%22agglo16_32_4_gt3um%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B8%2C8%2C40%5D%2C%22voxelCoordinates%22:%5B78997%2C25574%2C3600%5D%7D%7D%2C%22zoomFactor%22:13.1%7D%2C%22showAxisLines%22:false%2C%22showDefaultAnnotations%22:false%2C%22perspectiveOrientation%22:%5B0.08082044869661331%2C0.22225624322891235%2C-0.15153835713863373%2C-0.9597428441047668%5D%2C%22perspectiveZoom%22:2382%2C%22showSlices%22:false%2C%22layout%22:%223d%22%7D"
  sc2=ngl_decode_scene(singleton_url)
  sc2.rt=ngl_decode_scene(ngl_encode_url(sc2))
  expect_length(sc2.rt[['layers']][[1]][['segments']],2)
})


test_that('we can make a neuroglancer URL', {
  catmaid_url <- "https://fafb.catmaid.virtualflybrain.org/?pid=2&zp=131280&yp=170014.98879622458&xp=426584.81386896875&tool=navigator&sid0=2&s0=-1"
  open_fafb_ngl(catmaid_url, sampleurl = ngurl)
})
