test_that("flywire_sirepo_file", {
  skip_if_not_installed('git2r')
  expect_s3_class(
    flywire_sirepo_file("https://github.com/flyconnectome/flywire_annotations/supplemental_files/Supplemental_file3_summary_with_ngl_links.csv", read = T),
    "data.frame"
    )
})
