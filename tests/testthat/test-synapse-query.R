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

test_that("cave_bbox_split tiles a bounding box along its longest axis", {
  # y is the longest extent here (0..100 vs 0..10 in x and 0..40 in z)
  bb=rbind(c(0,0,0), c(10,100,40))

  # n<=1 returns the box unchanged, as a single-element list
  one=fafbseg:::cave_bbox_split(bb, 1L)
  expect_length(one, 1L)
  expect_equal(one[[1]], matrix(as.numeric(bb), 2, 3))

  # n=4 gives 4 contiguous slabs splitting the y axis, x/z untouched
  s=fafbseg:::cave_bbox_split(bb, 4L)
  expect_length(s, 4L)
  # x and z ranges are identical to the input in every slab
  for(sl in s) {
    expect_equal(sl[, 1], bb[, 1])
    expect_equal(sl[, 3], bb[, 3])
  }
  # y breaks are contiguous (each slab's max is the next slab's min) and span 0..100
  ymins=sapply(s, function(x) x[1, 2])
  ymaxs=sapply(s, function(x) x[2, 2])
  expect_equal(ymins, c(0, 25, 50, 75))
  expect_equal(ymaxs, c(25, 50, 75, 100))
  expect_equal(ymaxs[-length(ymaxs)], ymins[-1])

  # union of slabs reconstructs the original extent
  expect_equal(range(c(ymins, ymaxs)), c(bb[1, 2], bb[2, 2]))
})
