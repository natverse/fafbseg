skip_if(inherits(try(flywire_cave_client(), silent = T), 'try-error'),
        message = 'Skipping CAVE tests!')

test_that("cave query", {
  expect_message(res <- flywire_cave_query('nuclei_v1', datastack_name = 'flywire_fafb_production', limit=10, materialization_version = 349),
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
})

test_that("cave query", {
  expect_equal(as.numeric(flywire_timestamp(349)), 1650269400.14127)
})
