test_that("multiplication works", {
  ac=try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable tests as unable to login!")

  # sometimes the server seems to give up when requesting a token from each base
  fat <- try(flytable_alltables())
  skip_if(inherits(fat, 'try-error'),
          "skipping flytable tests as having trouble listing all tables!")

  expect_s3_class(mbons <- with_segmentation('flywire31', cam_meta('/cell_class:MBON', table = 'info', base='main')), 'data.frame')
  expect_true(nrow(mbons)>50)
  # now pick out one row where MBON was edited after 783 materialisation
  mbons.updated <- mbons[mbons$root_id!=mbons$root_783,]
  mbons.updated.1 <- mbons.updated[1,]
  # ... and check we can pull that up with the stale id
  expect_equal(with_segmentation('flywire31',
    cam_meta(mbons.updated.1$root_783, table = 'info', base='main',
             translate_ids = TRUE))$supervoxel_id,
    mbons.updated.1$supervoxel_id)

  # negative control: without translation the stale id does not match
  expect_true(is.na(with_segmentation('flywire31',
    cam_meta(mbons.updated.1$root_783, table = 'info', base='main',
             translate_ids = FALSE))$supervoxel_id))

  # the NA default auto-enables translation once a timestamp is supplied
  expect_equal(with_segmentation('flywire31',
    cam_meta(mbons.updated.1$root_783, table = 'info', base='main',
             timestamp = 'now'))$supervoxel_id,
    mbons.updated.1$supervoxel_id)

  # translate_ids must not perturb the query path
  expect_equal(with_segmentation('flywire31',
    sort(cam_meta('/cell_class:MBON', table = 'info', base='main',
                  translate_ids = TRUE)$root_id)),
    sort(mbons$root_id))
})
