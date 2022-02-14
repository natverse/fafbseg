test_that("fafb14_to_flywire_ids works", {
  skip_if_not(spine_ok())
  expect_equal(
    df <- fafb14_to_flywire_ids(5038796, conn = catmaid::vfbcatmaid()),
    data.frame(root_id = "720575940626699516", hits = 57L, skid = "5038796"))

  expect_equal(fafb14_to_flywire_ids(5038796, conn = catmaid::vfbcatmaid(),only.root = T
                                      )$root_id,
               df$root_id)
})
