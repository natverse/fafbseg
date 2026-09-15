test_that("cave_bbox_nm2vox converts nm bounding boxes to voxels", {
  bb=rbind(c(4e5, 1.6e5, 1e5), c(4.2e5, 1.8e5, 1.2e5))

  # nm table (vd=NULL) is an identity pass-through
  expect_equal(unname(fafbseg:::cave_bbox_nm2vox(bb)), unname(bb))

  # FAFB voxel resolution: nm -> voxels divides by (4,4,40)
  vd=c(4,4,40)
  expect_equal(
    unname(fafbseg:::cave_bbox_nm2vox(bb, vd=vd)),
    unname(sweep(bb, 2, vd, `/`))
  )

  # accepts a scatter of points and returns their enclosing 2x3 box
  pts=rbind(c(1,2,3), c(10,20,30), c(5,5,5))
  expect_equal(dim(fafbseg:::cave_bbox_nm2vox(pts)), c(2L, 3L))

  # rejects things that are not a 2x3 box
  expect_error(fafbseg:::cave_bbox_nm2vox(rbind(c(1,2), c(3,4))))
})
