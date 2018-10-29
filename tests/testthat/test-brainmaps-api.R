context("test-brainmaps-api")

test_that("parse brainmaps uri works", {

  expect_error(parse_brainmaps_uri("wurgle"))
  expect_error(parse_brainmaps_uri("brainmaps://"))

  uri = "brainmaps://772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2_flatreseg2_skeletons32/teasar512_nnconn165_mc10000_prune10_thresh1000_sparse250"
  res = list(volume = "772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2_flatreseg2_skeletons32",
             meshName = "teasar512_nnconn165_mc10000_prune10_thresh1000_sparse250")
  class(res) = 'brainmaps_uri'
  expect_equal(parse_brainmaps_uri(uri, mesh_required = TRUE), res)

  uri2 = "brainmaps://772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2_flatreseg2_skeletons32"
  res2=res
  res2$meshName=NULL
  expect_equal(parse_brainmaps_uri(uri2, mesh_required = FALSE), res2)
  expect_error(parse_brainmaps_uri(uri2, mesh_required = TRUE))

})

test_that("brainmaps_volume works",{
  vol="772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2"
  uri=paste0("brainmaps://",vol,'/meshname')
  expect_equal(brainmaps_volume(vol), vol)
  expect_equal(brainmaps_volume(uri), vol)
  expect_equal(brainmaps_volume(parse_brainmaps_uri(uri)), vol)
})
