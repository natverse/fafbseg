test_that("flywire_user_info works", {
  # requires admin privileges since middle_auth v2.29.0 (April 2025)
  res <- tryCatch(
    flywire_user_info(60, datastack_name = "flywire_fafb_public"),
    error = function(e) e
  )
  skip_if(inherits(res, "error") && grepl("403|admin", conditionMessage(res)),
          "Requires admin privileges to look up user info")
  expect_s3_class(res, "data.frame")
  expect_match(res$name, 'Jefferis')
  expect_equal(
    flywire_user_info(data.frame(user_id=60), "flywire_fafb_public"),
    res)
})
