test_that("reading a file works", {
  skip_if_not_installed('git2r')
  expect_is(anns <- flywire_sirepo_file('supplemental_files/Supplemental_file1_annotations.tsv', read = TRUE, version=630), 'data.frame')
  expect_type(
    flywire_sirepo_file('https://github.com/flyconnectome/flywire_annotations/blob/staging/supplemental_files/Supplemental_file2_non_neuron_annotations.tsv'),
    'character'
  )
})
