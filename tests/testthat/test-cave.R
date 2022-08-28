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

})

test_that("flywire_timestamp", {
  expect_equal(as.numeric(flywire_timestamp(349)), 1650269400.14127)
  expect_equal(flywire_timestamp(349),
               flywire_timestamp(timestamp = 1650269400.14127))
  expect_equal(flywire_timestamp(timestamp = "2022-04-18 08:10:00 UTC"),
               flywire_timestamp(timestamp = 1650269400))
  expect_warning(flywire_timestamp(timestamp = "2022-04-18 08:10:00"))
  expect_error(flywire_timestamp(1, 2))
  expect_error(flywire_timestamp())
})

