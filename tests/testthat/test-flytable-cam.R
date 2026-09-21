test_that("status_matches handles case and multi-select tokens", {
  # single lowercase token (aedes-style)
  expect_equal(status_matches(c("duplicate", "traced", NA, ""),
                              c("duplicate", "bad_nucleus")),
               c(TRUE, FALSE, FALSE, FALSE))
  # capitalised, comma-separated multi-select (CRANT-style)
  expect_equal(status_matches(c("BACKBONE_PROOFREAD,DUPLICATED",
                                "PARTIALLY_PROOFREAD",
                                "DUPLICATED"),
                              "DUPLICATED"),
               c(TRUE, FALSE, TRUE))
  # case-insensitive both ways, whitespace around tokens tolerated
  expect_equal(status_matches("Backbone, Duplicated", "duplicated"),
               TRUE)
  # empty drop set keeps everything
  expect_equal(status_matches(c("DUPLICATED", "x"), character(0)),
               c(FALSE, FALSE))
})

test_that("cam_parse_ids expands id lists and neuroglancer URLs", {
  ids <- c("720575940625862972", "720575940625862974")
  expect_equal(cam_parse_ids("720575940625862972, 720575940625862974"), ids)
  expect_equal(cam_parse_ids(" 720575940625862972,720575940625862974 \n"), ids)
  expect_equal(cam_parse_ids("720575940625862972 720575940625862974"), ids)

  # passed through untouched
  expect_identical(cam_parse_ids("class:ALPN"), "class:ALPN")
  expect_identical(cam_parse_ids("MBON.+"), "MBON.+")
  expect_identical(cam_parse_ids(ids[1]), ids[1])
  expect_identical(cam_parse_ids(ids), ids)
  expect_null(cam_parse_ids(NULL))

  # full-state URL decodes offline; hidden (!-prefixed) segments are dropped
  j <- paste0('{"layers":[{"type":"segmentation","source":"graphene://x",',
              '"segments":["720575940625862972","!720575940625862973",',
              '"720575940625862974"],"name":"seg"}]}')
  u <- paste0("https://spelunker.cave-explorer.org/#!",
              utils::URLencode(j, reserved = TRUE))
  expect_equal(cam_parse_ids(u), ids)
})

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
