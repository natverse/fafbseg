context("test-ids")

test_that("simple ids", {
  # temporarily use test zip folder
  op <- options(fafbseg.skelziproot = 'testdata/skeletonzip/')
  on.exit(options(op))

  expect_equal(segmentid2zip(10001654273), "10001.zip")

  baseline = structure(
    c(10001654273, 10001654273, 10001654273, 0, 1, 2),
    .Dim = 3:2,
    .Dimnames = list(NULL, c("segment", "fragment"))
  )
  expect_equal(swc2segmentid(sprintf("10001654273.%d.swc", 0:2), include.fragment=TRUE),
               baseline)

  expect_equal(segmentid2zip(swc2segmentid("10001654273.1.swc")), "10001.zip")

  # check for error when we specify a non-existent folder
  options(fafbseg.skelziproot = tempfile())
  expect_error(segmentid2zip(10001654273))
})

test_that('valid_id', {
  expect_equal(valid_id(c(2^54, 1, 0, -1, NA)), c(F, T, T, F, F))
  expect_equal(valid_id(c(2^54, 1, 0, -1, NA), na.ok = T), c(F, T, T, F, T))

  expect_true(valid_id(NA, na.ok=T))
  expect_equal(valid_id(c(1, 0, "NAN", "NULL", "-1"), na.ok=T), c(T,T,T,T,F))
})

test_that("flywire_ids",{
  baseline=c(0,0,0,1:4)
  expect_equal(i64=flywire_ids(c(NA, -1:4), integer64 = T, must_work = F),
               id2char(baseline))
})

test_that('ngl_segments', {
  expect_equal(ngl_segments(1e5), '100000')

  baseline=as.character(c(10950626347, 10952282491, 13307888342))

  expect_equal(ngl_segments(baseline), baseline)
  expect_equal(ngl_segments(baseline, must_work = TRUE), baseline)
  expect_equal(ngl_segments(baseline, as_character = F), as.numeric(baseline))

  expect_equal(ngl_segments(c(1,2,1), as_character = F), c(1,2,1))
  expect_equal(expect_warning(ngl_segments(c(1,2,1), as_character = F, unique = T)),
               1:2)

  expect_error(ngl_segments(numeric(), must_work = T),
               regexp = "no valid")
  expect_error(ngl_segments(numeric(), must_work = T, as_character = F))
  expect_error(ngl_segments(character(), must_work = T),
               regexp = "no valid")
  expect_error(ngl_segments(c("-1", 4, 5), must_work = T))
  expect_error(ngl_segments(c("-1", 4, 5), must_work = T, as_character = F))

  expect_equal(ngl_segments(c(NA, 4, 5), must_work = F, as_character = F),
               c(0, 4, 5))
  expect_equal(ngl_segments(c(NA, 4, 5), must_work = F, as_character = T),
               c("0", "4", "5"))

  # json file
  expect_equal(ngl_segments("testdata/testscene.json"), baseline)
  # json text
  # this now fails, but I don't
  expect_equal(ngl_segments(readLines("testdata/testscene.json")), baseline)
  # R list
  scene=jsonlite::fromJSON("testdata/testscene.json")
  expect_equal(ngl_segments(scene), baseline)
  expect_equal(names(ngl_layers(ngl_decode_scene(scene))), as.character(1:2))
  expect_equal(ngl_segments(ngl_encode_url(body = scene)), baseline)

  # list with two sets of segments
  expect_warning(ngl_segments("testdata/testscene-double.json"))

  # no segments
  u="https://ngl.flywire.ai/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked%22%2C%22type%22:%22image%22%2C%22blend%22:%22default%22%2C%22shaderControls%22:%7B%7D%2C%22name%22:%22Sandbox-image%22%7D%2C%7B%22source%22:%22graphene://https://prodv1.flywire-daf.com/segmentation/table/fly_v26%22%2C%22type%22:%22segmentation_with_graph%22%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22graphOperationMarker%22:%5B%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%2C%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%5D%2C%22pathFinder%22:%7B%22color%22:%22#ffff00%22%2C%22pathObject%22:%7B%22annotationPath%22:%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%2C%22hasPath%22:false%7D%7D%2C%22name%22:%22sandbox-segmentation-FOR%20PRACTICE%20ONLY%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B108360%2C42086%2C3279%5D%7D%7D%2C%22zoomFactor%22:4%7D%2C%22perspectiveOrientation%22:%5B-0.0021646153181791306%2C0.000400538498070091%2C8.670138527122617e-7%2C0.9999975562095642%5D%2C%22perspectiveZoom%22:2585.0186809766333%2C%22jsonStateServer%22:%22https://globalv1.flywire-daf.com/nglstate/post%22%2C%22selectedLayer%22:%7B%22layer%22:%22sandbox-segmentation-FOR%20PRACTICE%20ONLY%22%7D%2C%22layout%22:%22xy-3d%22%7D"

  expect_error(ngl_segments(u, must_work = TRUE))
  expect_equal(ngl_segments(u, must_work = FALSE), character())
  expect_equal(ngl_segments(u, must_work = FALSE, as_character = FALSE), numeric())
  sc=ngl_decode_scene(u)
  # replace with segments in scene
  expect_silent(ngl_segments(sc) <- scene)
  expect_equal(ngl_segments(sc), baseline)
  expect_is(sc, 'ngscene')
  expect_silent(ngl_segments(sc) <- as.numeric(baseline))
  expect_equal(ngl_segments(sc), baseline)

  # replace URL directly
  ngl_segments(u) <- 1:4
  expect_is(u, 'character')
  expect_equal(ngl_segments(u), as.character(1:4))
})


test_that('flywire segments', {
  fcurl="https://neuromancer-seung-import.appspot.com/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://microns-seunglab/drosophila_v0/alignment/vector_fixer30_faster_v01/v4/image_stitch_v02%22%2C%22type%22:%22image%22%2C%22blend%22:%22default%22%2C%22shaderControls%22:%7B%7D%2C%22name%22:%22Image%22%7D%2C%7B%22source%22:%22graphene://https://fafbv2.dynamicannotationframework.com/segmentation/1.0/fly_v26%22%2C%22type%22:%22segmentation_with_graph%22%2C%22selectedAlpha%22:0.1%2C%22segmentColors%22:%7B%22720575940615118379%22:%22#f8ff79%22%2C%22720575940639495437%22:%22#97ff32%22%7D%2C%22segments%22:%5B%22720575940609057772%22%2C%22720575940628716862%22%2C%22720575940631951940%22%5D%2C%22hiddenSegments%22:%5B%22720575940415076500%22%2C%22720575940597502225%22%2C%22720575940597504529%22%2C%22720575940607945290%22%2C%22720575940607945546%22%2C%22720575940609059820%22%2C%22720575940609464529%22%2C%22720575940610191025%22%2C%22720575940611936689%22%2C%22720575940611938993%22%2C%22720575940611957425%22%2C%22720575940612060849%22%2C%22720575940612306609%22%2C%22720575940612630330%22%2C%22720575940612632122%22%2C%22720575940612639290%22%2C%22720575940612639802%22%2C%22720575940612923706%22%2C%22720575940612924218%22%2C%22720575940612924474%22%2C%22720575940612926266%22%2C%22720575940612926522%22%2C%22720575940612928570%22%2C%22720575940612932666%22%2C%22720575940612932922%22%2C%22720575940612943674%22%2C%22720575940612945722%22%2C%22720575940612945978%22%2C%22720575940612946234%22%2C%22720575940612947514%22%2C%22720575940612947770%22%2C%22720575940612948794%22%2C%22720575940612949050%22%2C%22720575940612949562%22%2C%22720575940612949818%22%2C%22720575940612950586%22%2C%22720575940612965434%22%2C%22720575940612968762%22%2C%22720575940612969786%22%2C%22720575940612975418%22%2C%22720575940613419697%22%2C%22720575940614447674%22%2C%22720575940614476090%22%2C%22720575940615935802%22%2C%22720575940616167979%22%2C%22720575940616245547%22%2C%22720575940616339066%22%2C%22720575940616858426%22%2C%22720575940616877370%22%2C%22720575940616974906%22%2C%22720575940622666491%22%2C%22720575940623916973%22%2C%22720575940623917997%22%2C%22720575940623988909%22%2C%22720575940623990445%22%2C%22720575940624091309%22%2C%22720575940624091821%22%2C%22720575940624092077%22%2C%22720575940624755262%22%2C%22720575940625276333%22%2C%22720575940625278893%22%2C%22720575940625279149%22%2C%22720575940628256941%22%2C%22720575940628257965%22%2C%22720575940628258221%22%2C%22720575940628258733%22%2C%22720575940628259245%22%2C%22720575940628260013%22%2C%22720575940628260269%22%2C%22720575940628388525%22%2C%22720575940629887774%22%2C%22720575940630320958%22%2C%22720575940631495069%22%2C%22720575940631627421%22%2C%22720575940631629213%22%2C%22720575940631634589%22%2C%22720575940631636381%22%2C%22720575940631642525%22%2C%22720575940631642781%22%2C%22720575940631643805%22%2C%22720575940631644317%22%2C%22720575940631646365%22%2C%22720575940631654045%22%2C%22720575940632422317%22%2C%22720575940632474541%22%2C%22720575940633147166%22%2C%22720575940639460877%22%2C%22720575940639491341%22%2C%22720575940639495693%22%2C%22720575940639496461%22%2C%22720575940639496717%22%2C%22720575940639498253%22%2C%22720575940639520525%22%2C%22720575940639522317%22%2C%22720575940643008070%22%2C%22720575940643008326%22%2C%22720575940644836166%22%2C%22720575940644949830%22%2C%22720575940648192838%22%2C%22720575940651478036%22%2C%22720575940651644436%22%5D%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22graphOperationMarker%22:%5B%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%2C%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%5D%2C%22name%22:%22SANDBOX_CHANGES%20NOT%20SAVED%20TO%20REAL%20DATASET%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B116967.84375%2C36816.7734375%2C2359.56982421875%5D%7D%7D%2C%22zoomFactor%22:3.7444200637176586%7D%2C%22perspectiveOrientation%22:%5B0.08711250126361847%2C0.1580400913953781%2C0.2011885792016983%2C0.9627865552902222%5D%2C%22perspectiveZoom%22:2611.3356233893137%2C%22showSlices%22:false%2C%22gpuMemoryLimit%22:2000000000%2C%22jsonStateServer%22:%22https://fafbv2.dynamicannotationframework.com/nglstate/post%22%2C%22selectedLayer%22:%7B%22layer%22:%22SANDBOX_CHANGES%20NOT%20SAVED%20TO%20REAL%20DATASET%22%2C%22visible%22:true%7D%2C%22layout%22:%22xy-3d%22%7D"
  sc=ngl_decode_scene(fcurl)
  baseline=c(
    "720575940609057772",
    "720575940628716862",
    "720575940631951940"
  )
  expect_equal(
    ngl_segments(sc, as_character = T, include_hidden = F),
    baseline
  )

  expect_equal(ngl_segments(sc-"720575940631951940"), baseline[1:2])
  expect_equal(ngl_segments(sc-"720575940628716862"), baseline[-2])
  expect_equal(ngl_segments((sc-"720575940631951940")+"720575940631951940"),
    baseline)
  expect_equal(ngl_segments((sc-"720575940628716862")+"720575940628716862"),
               baseline[c(1,3,2)])
  expect_equal(ngl_segments(sc-"0"), baseline)
  expect_equal(ngl_segments(sc-0), baseline)
  # make sure we can remove all segments
  expect_silent(sc-ngl_segments(sc))
  expect_silent(ngl_segments(sc) <- numeric())
  expect_silent(ngl_segments(sc) <- character())
  expect_silent(ngl_segments(sc) <- NULL)

  # Incompatible methods ("-.ngscene", "-.integer64") for "-"
  # expect_equal(ngl_segments(sc - bit64::as.integer64(0) ), baseline)

  sc=ngl_decode_scene(fcurl)
  expect_equal(
    ngl_segments(sc, as_character = T, include_hidden = TRUE),
    c(sc$layers[[2]]$segments, sc$layers[[2]]$hiddenSegments)
  )
})


test_that('neuroglancer segmentation for all built-in urls',  {
  releases=eval(formals(choose_segmentation)[['release']])
  for(r in releases) {
    with_segmentation(r,
      {
        expect_is(ngl_segmentation(rval='url'), 'character')
        expect_is(ngl_segmentation(rval='full'), 'list')
      })
    }
})


test_that('layer manipulation',{
  f=system.file("flywire-annotations.json" , package = 'fafbseg')
  scene=ngl_decode_scene(f)
  scene2=scene
  ngl_layers(scene2) <- ngl_layers(scene)
  expect_equal(scene, scene2)
  ngl_layers(scene2) <- rev(rev(ngl_layers(scene)))
  expect_equal(scene, scene2)

  ngl_layers(scene2)[[4]]=NULL
  ngl_layers(scene)=ngl_layers(scene)[1:3]
  expect_equal(scene, scene2)

  scene=ngl_decode_scene(f)
  expect_equal(
    (scene - "jfrc_mesh_test") + ngl_layers(scene)['jfrc_mesh_test'],
    scene)

  brainmesh='{"source":"precomputed://https://spine.janelia.org/files/eric/jfrc_mesh_test","type":"segmentation","objectAlpha":0.25,"loadSkeletons":false,"segments":["1"],"hiddenSegments":["2"],"skeletonRendering":{"mode2d":"lines_and_points","mode3d":"lines"},"name":"jfrc_mesh_test",
  "visible":false}'

  expect_equal(
    (scene - "jfrc_mesh_test") + brainmesh,
    scene)
})
