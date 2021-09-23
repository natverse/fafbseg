test_that("fafb14_to_flywire_ids works", {
  expect_equal(
    df <- fafb14_to_flywire_ids(5038796, conn = catmaid::vfbcatmaid()),
    data.frame(flywire.id = "720575940636059967", hits = 57L, skid = "5038796"))

  expect_equal(fafb14_to_flywire_ids(5038796, conn = catmaid::vfbcatmaid(),only.root = T
                                      )$flywire.id,
               df$flywire.id)
})
