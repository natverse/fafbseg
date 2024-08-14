context("test-urls")

# utils::URLencode has subtle differences from curl
ngurl="https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:[%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb14_4nm_split4x_fill2:ffnreseg_whitened_16nm_32nm_4nm%22,%22type%22:%22segmentation%22,%22segments%22:[%220%22,%223140809165%22,%224193866734%22,%224256801664%22,%224314929385%22,%224373082358%22,%224610419193%22,%224672751347%22],%22name%22:%22agglo16_32_4_gt3um%22%7D],%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:[8,8,40],%22voxelCoordinates%22:[78997,25574,3600]%7D%7D,%22zoomFactor%22:13.1%7D,%22showAxisLines%22:false,%22showDefaultAnnotations%22:false,%22perspectiveOrientation%22:[0.08,0.22,-0.15,-0.95],%22perspectiveZoom%22:2382,%22showSlices%22:false,%22layout%22:%223d%22%7D"

# this uses the new urlencode function and is confirmed to work ...
ngurl="https://neuroglancer-demo.appspot.com/#!%7B%22layers%22%3A%5B%7B%22source%22%3A%22brainmaps%3A%2F%2F772153499790%3Afafb_v14%3Afafb14_4nm_split4x_fill2%3Affnreseg_whitened_16nm_32nm_4nm%22%2C%22type%22%3A%22segmentation%22%2C%22segments%22%3A%5B%220%22%2C%223140809165%22%2C%224193866734%22%2C%224256801664%22%2C%224314929385%22%2C%224373082358%22%2C%224610419193%22%2C%224672751347%22%5D%2C%22name%22%3A%22agglo16_32_4_gt3um%22%7D%5D%2C%22navigation%22%3A%7B%22pose%22%3A%7B%22position%22%3A%7B%22voxelSize%22%3A%5B8%2C8%2C40%5D%2C%22voxelCoordinates%22%3A%5B78997%2C25574%2C3600%5D%7D%7D%2C%22zoomFactor%22%3A13.1%7D%2C%22showAxisLines%22%3Afalse%2C%22showDefaultAnnotations%22%3Afalse%2C%22perspectiveOrientation%22%3A%5B0.08%2C0.22%2C-0.15%2C-0.95%5D%2C%22perspectiveZoom%22%3A2382%2C%22showSlices%22%3Afalse%2C%22layout%22%3A%223d%22%7D"


test_that("decode scene works", {
  expect_error(ngl_decode_scene('https'))
  expect_error(ngl_decode_scene('{'))

  # url="https://neuroglancer-demo.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22brainmaps://772153499790:fafb_v14:fafb14_4nm_split4x_fill2:ffnreseg_whitened_16nm_32nm_4nm%22%2C%22type%22:%22segmentation%22%2C%22segments%22:%5B%220%22%2C%223140809165%22%2C%224193866734%22%2C%224256801664%22%2C%224314929385%22%2C%224373082358%22%2C%224610419193%22%2C%224672751347%22%5D%2C%22name%22:%22agglo16_32_4_gt3um%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B8%2C8%2C40%5D%2C%22voxelCoordinates%22:%5B78997.46875%2C25574.220703125%2C3600.365966796875%5D%7D%7D%2C%22zoomFactor%22:13.123994088552514%7D%2C%22showAxisLines%22:false%2C%22showDefaultAnnotations%22:false%2C%22perspectiveOrientation%22:%5B0.08349629491567612%2C0.2258872538805008%2C-0.15645836293697357%2C-0.9578747749328613%5D%2C%22perspectiveZoom%22:2382.282469476382%2C%22showSlices%22:false%2C%22layout%22:%223d%22%7D"
  # after url decoding, reducing precision and re-encoding

  expect_is(sc <- ngl_decode_scene(ngurl), "ngscene")
  expect_known_value(sc, file = testthat::test_path('testdata/ngscene.rds'))
  with_segmentation("20190805", expect_equal(ngl_encode_url(sc), ngurl))
  # expect_is(json <- ngl_decode_scene(ngurl, return.json = TRUE), "character")
  # writeLines(json, con='tests/testthat/testdata/ngurl.json')
  expect_equal(ngl_decode_scene(ngurl, return.json = TRUE),
               readLines('testdata/ngurl.json'))

  # check we can cope with a scene as input as a convenience
  expect_equal(ngl_decode_scene(sc), sc)

  # check that we can cope with a scene that contains state server details

  u="https://neuromancer-seung-import.appspot.com/#!%7B%22layers%22:[%7B%22source%22:%22precomputed://gs://zetta_lee_fly_vnc_001_precomputed/fanc_v4_em%22,%22type%22:%22image%22,%22blend%22:%22default%22,%22shaderControls%22:%7B%7D,%22name%22:%22FANCv4%22%7D,%7B%22source%22:%22graphene://https://cave.fanc-fly.com/segmentation/table/mar2021_prod%22,%22type%22:%22segmentation_with_graph%22,%22colorSeed%22:1792288153,%22segmentColors%22:%7B%22648518346498254576%22:%22#1fe0f9%22%7D,%22segments%22:[%22648518346498254576%22],%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22,%22mode3d%22:%22lines%22%7D,%22graphOperationMarker%22:[%7B%22annotations%22:[],%22tags%22:[]%7D,%7B%22annotations%22:[],%22tags%22:[]%7D],%22pathFinder%22:%7B%22color%22:%22#ffff00%22,%22pathObject%22:%7B%22annotationPath%22:%7B%22annotations%22:[],%22tags%22:[]%7D,%22hasPath%22:false%7D%7D,%22name%22:%22seg_Mar2021_proofreading%22%7D,%7B%22source%22:%22precomputed://gs://lee-lab_female-adult-nerve-cord/alignmentV4/synapses/postsynapses_May2021%22,%22type%22:%22image%22,%22blend%22:%22default%22,%22shader%22:%22void%20main()%20%7B%20emitRGBA(vec4(1,%200,%201,%20toNormalized(getDataValue())));%20%7D%22,%22shaderControls%22:%7B%7D,%22name%22:%22synapses_May2021%22,%22visible%22:false%7D,%7B%22type%22:%22segmentation%22,%22mesh%22:%22precomputed://gs://zetta_lee_fly_vnc_001_precomputed/vnc1_full_v3align_2/brain_regions%22,%22objectAlpha%22:0.1,%22hideSegmentZero%22:false,%22ignoreSegmentInteractions%22:true,%22segmentColors%22:%7B%221%22:%22#bfbfbf%22,%222%22:%22#d343d6%22%7D,%22segments%22:[%221%22,%222%22],%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22,%22mode3d%22:%22lines%22%7D,%22name%22:%22volume%20outlines%22%7D],%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:[4.300000190734863,4.300000190734863,45],%22voxelCoordinates%22:[48848.171875,114737.2109375,2690]%7D%7D,%22zoomFactor%22:11.839474231467724%7D,%22perspectiveZoom%22:6704.002738252677,%22showSlices%22:false,%22gpuMemoryLimit%22:4000000000,%22systemMemoryLimit%22:4000000000,%22concurrentDownloads%22:64,%22jsonStateServer%22:%22https://global.daf-apis.com/nglstate/api/v1/post%22,%22selectedLayer%22:%7B%22layer%22:%22seg_Mar2021_proofreading%22,%22visible%22:true%7D,%22layout%22:%22xy-3d%22%7D"

  expect_s3_class(sc <- ngl_decode_scene(u), 'ngscene')
  expect_s3_class(ngl_decode_scene("https://tinyurl.com/rmr58jpn"), 'ngscene')
  expect_s3_class(ngl_decode_scene("https://neuroglancer-demo.appspot.com/#!gs://flyem-user-links/short/2023-08-26.151006.json"), 'ngscene')
  expect_s3_class(ngl_decode_scene("https://tinyurl.com/flywirehb2"), 'ngscene')
  expect_type(ngl_decode_scene("https://tinyurl.com/flywirehb2", return.json = T), 'character')
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

  # check we use original baseurl when available
  fu=flywire_expandurl('https://tinyurl.com/rmr58jpn')
  hostname <- function(x) httr::parse_url(x)$hostname
  expect_equal(hostname(as.character(ngl_decode_scene(fu))),
               hostname(fu))
})

test_that('we can expand neuroglancer URLs without following', {
  expect_equal(
    flywire_expandurl('https://tinyurl.com/NeckConnective', follow = FALSE),
    "https://neuroglancer-demo.appspot.com/#!gs://flyem-user-links/short/NeckConnective.json"
  )
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

  # FIXME want to skip on mac on ci only
  skip_on_os('mac')

  u1="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/6528186495008768"
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


test_that('we can extract ids from delimited strings',{
  expect_equal(flywire_ids('1234, 12345', integer64 = T),
               c("1234", "12345"))
  expect_equal(flywire_ids('1234, 12345', integer64 = T),
               bit64::as.integer64(c("1234", "12345")))

  expect_equal(flywire_ids('1234,   12345 ', integer64 = T),
               c("1234", "12345"))
  expect_equal(flywire_ids(' 1234   12345', integer64 = T),
               c("1234", "12345"))
  expect_equal(flywire_ids('\t 1234 \t\n  12345', integer64 = T),
               c("1234", "12345"))
  expect_equal(flywire_ids('\t fw:1234 \t\n  fw:12345', integer64 = T),
               c("1234", "12345"))

  ids='720575940628529156,720575940621343749,720575940628007172,720575940625455370'
  bl=c("720575940628529156", "720575940621343749", "720575940628007172",
       "720575940625455370")

  expect_equal(flywire_ids(ids), bl)
  tf=tempfile('ids', fileext = '.txt')
  on.exit(unlink(tf))
  writeLines(ids, tf)
  expect_equal(flywire_ids(file = tf), bl)
  writeLines(bl, tf)
  expect_equal(flywire_ids(file = tf), bl)

  expect_warning(flywire_ids(1, file = tf), regexp = 'only')
  expect_error(flywire_ids(file = tempfile()), regexp = 'file')
})
