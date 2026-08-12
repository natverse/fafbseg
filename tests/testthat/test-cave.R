skip_if_flywire_materialize_unavailable(
  message = "Skipping CAVE tests: FlyWire materialize service unavailable"
)

test_that("cave query", {
  expect_message(res <- flywire_cave_query('nuclei_v1', datastack_name = 'flywire_fafb_production', limit=10, version = 349),
            'no longer available')
  expect_equal(
    c(
      7393349L,
      7416439L,
      7415038L,
      7415013L,
      7415848L,
      7415851L,
      7415718L,
      7415838L,
      7415441L,
      4282686L
    ),
    res$id
  )

  expect_message(res2 <- flywire_cave_query('nuclei_v1', datastack_name = 'flywire_fafb_production', version = 349, filter_in_dict = list(id=7393349)),
                 'no longer available')
  expect_equal(
    res2, res[1,]
  )

  expect_true(nrow(pnv10 <- flywire_cave_query('proofread_neurons_view', limit = 10))==10)
  expect_equal(
    flywire_cave_query('proofread_neurons_view', limit = 10, version = 'latest'),
    pnv10)

  expect_error(flywire_cave_query('proofread_neurons_view', limit = 10, timestamp = 'now'))

  expect_warning(expect_s3_class(class = 'data.frame',
    mbon012 <- flywire_cave_query("cambridge_celltypes_v2", version=783, timetravel = T,
                       filter_regex_dict = c(tag='MBON0[12]'))
    ))

  expect_in(mbon012$pt_root_id,
            mbon012.ids <- flywire_ids('/type:MBON0[12]', version = 783, use_static=T))

  expect_in(flywire_cave_query('cambridge_celltypes_v2',
                     filter_in_dict = list(pt_root_id=mbon012.ids), version=783L)$tag,
            c("MBON01", "MBON02"))

  expect_in(flywire_cave_query(
    table = 'cambridge_celltypes_v2',
    filter_in_dict = list(pt_root_id=mbon012.ids),
    version=783L, live = 2, allow_missing_lookups=T)$tag,
            c("MBON01", "MBON02"))

  expect_warning(expect_error(
    flywire_cave_query("cambridge_celltypes_v2", version=783, timetravel = T,
      filter_regex_dict = c(tag='MBON0[12]'),
      select_columns = c("id", "pt_root_id", "tag"))
  ))
  # see https://flywire-forum.slack.com/archives/C01M4LP2Y2D/p1757233474782829
  # created, deleted
  expect_silent(
    flywire_cave_query("cambridge_celltypes_v2", version=783, timetravel = T,
                       filter_regex_dict = list(cambridge_celltypes_v2=list(tag='MBON0[12]')),
                       select_columns = list(cambridge_celltypes_v2=c("id", "pt_root_id", "pt_supervoxel_id","tag", "created", "deleted")))
  )

})

test_that("flywire_timestamp", {
  expect_equal(as.numeric(flywire_timestamp(349)), 1650269400.14127)
  expect_equal(flywire_timestamp(349),
               flywire_timestamp(timestamp = 1650269400.14127))
  expect_equal(flywire_timestamp(timestamp = "2022-04-18 08:10:00 UTC"),
               flywire_timestamp(timestamp = 1650269400))
  expect_warning(flywire_timestamp(timestamp = "2022-04-18 08:10:00"))
  expect_error(flywire_timestamp(1, 2))
  expect_null(flywire_timestamp())
  # now -> current time, convert=F python object
  expect_is(flywire_timestamp(timestamp = 'now', convert = F), "datetime.date")
})

test_that("drop_if_row_limited only discards genuine row limits (#246)", {
  # No warning: result passes through.
  expect_equal(drop_if_row_limited(data.frame(a = 1:2)), data.frame(a = 1:2))

  # A row limit discards the result and says so.
  limited <- function() {
    warning("201 - \"Limited query to 5 rows")
    data.frame(a = 1)
  }
  expect_warning(res <- drop_if_row_limited(limited()),
                 "exceeded row limit")
  expect_null(res)

  # An unrelated warning keeps the result and is passed on rather than being
  # relabelled as a row limit. This is the pandas deprecation case.
  noisy <- function() {
    warning("FutureWarning: Index.format is deprecated")
    data.frame(a = 1:3)
  }
  expect_warning(res <- drop_if_row_limited(noisy()), "Index.format")
  expect_equal(nrow(res), 3L)

  # Both kinds together: still treated as a row limit.
  both <- function() {
    warning("FutureWarning: Index.format is deprecated")
    warning("row limit reached")
    data.frame(a = 1)
  }
  expect_null(suppressWarnings(drop_if_row_limited(both())))
})
