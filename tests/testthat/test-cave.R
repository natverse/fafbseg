test_that("cave query", {
  expect_warning(res <- flywire_cave_query('nuclei_v1', datastack_name = 'flywire_fafb_production', limit=10, materialization_version = 349))
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
