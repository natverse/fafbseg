test_that("flywire_user_info works", {
  expect_s3_class(rdf <- flywire_user_info(60, datastack_name = "flywire_fafb_public"),
                  "data.frame")
  expect_match(rdf$name, 'Jefferis')
  expect_equal(
    flywire_user_info(data.frame(user_id=60), "flywire_fafb_public"),
    rdf)
})
