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

test_that("cam_meta pins 'now' once and maps explicit ids only once", {
  ts <- list()
  record <- function(x, timestamp = NULL, ...) {
    ts[[length(ts) + 1]] <<- timestamp
    sub("^10$", "11", x)
  }
  local_mocked_bindings(
    .cam_idcache = new.env(parent = emptyenv()),
    flytable_cached_table = function(...)
      data.frame(`_id` = "a", root_id = "10", supervoxel_id = "1",
                 status = NA_character_, check.names = FALSE),
    flywire_updateids = record,
    flywire_latestid = record
  )
  res <- cam_meta(c("10", "99"), timestamp = "now")
  expect_equal(res$root_id, c("11", "99"))
  # one table remap + one id translation, sharing a single resolved timestamp
  expect_length(ts, 2L)
  expect_s3_class(ts[[1]], "POSIXct")
  expect_identical(ts[[1]], ts[[2]])

  # a query at the same time maps its rows from the cache, with no new lookup
  res <- cam_meta("root_id:10", timestamp = ts[[1]])
  expect_equal(res$root_id, "11")
  expect_length(ts, 2L)
})

test_that("cam_updateids follows a real edit like flywire_updateids", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if_not_installed('reticulate')
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  local_mocked_bindings(.cam_idcache = new.env(parent = emptyenv()))
  # DL1 PNs (version 401 roots); one was edited at 2022-06-14 10:27:18 UTC
  ids = c("720575940618302936","720575940627042064","720575940618757681",
          "720575940632167085")
  svids = c("77337105814452184","80857191821269694","78534748452308679",
            "80927491845825492")
  t0 = as.POSIXct("2022-06-14 09:00:00", tz = "UTC")
  r0 = try(cam_updateids(ids, svids, timestamp = t0), silent = TRUE)
  skip_if(inherits(r0, "try-error"), "Skipping live flywire tests")
  # a later call updates the cached roots retired by the edit
  r1 = cam_updateids(ids, svids, timestamp = t0 + 7200)
  expect_equal(r1, flywire_updateids(ids, svids, timestamp = t0 + 7200))
  expect_false(identical(r0, r1))
  # an earlier time, or a gap of over a day, falls back to a full lookup
  expect_equal(cam_updateids(ids, svids, timestamp = t0), r0)
  t2 = t0 + 7200 + 2*86400
  expect_equal(cam_updateids(ids, svids, timestamp = t2),
               flywire_updateids(ids, svids, timestamp = t2))
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
