test_that("reading a file works", {
  skip_if_not_installed('git2r')
  expect_is(anns <- flywire_sirepo_file('supplemental_files/Supplemental_file1_annotations.tsv',read = TRUE), 'data.frame')
})
