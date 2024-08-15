skip_if(inherits(try(flywire_cave_client(), silent = T), 'try-error'),
        message = 'Skipping CAVE tests!')

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

  expect_silent(
    flywire_cave_query("cambridge_celltypes_v2", version=783, timetravel = T,
                       filter_regex_dict = list(cambridge_celltypes_v2=list(tag='MBON0[12]')),
                       select_columns = list(cambridge_celltypes_v2=c("id", "pt_root_id", "pt_supervoxel_id","tag")))
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

