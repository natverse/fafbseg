test_that("flywire_partner_summary works", {
  expect_message(df <- flywire_partner_summary("720575940615237849", Verbose = T),
                'Fetching supervoxel.*720575940615237849')
  expect_is(df, 'data.frame')
  expect_named(df, c("post_id", "weight", "n"))
  expect_message(df2 <- flywire_partner_summary("720575940615237849", partners = 'input', Verbose = T),
                 'Fetching supervoxel.*720575940615237849')

})

test_that("flywire_ntpred+flywire_ntplot works", {
  ntp <- try(flywire_ntpred("720575940615237849"), silent = TRUE)
  skip_if(inherits(ntp, 'try-error'),
          'No neurotransmitter prediction data available')
  expect_is(ntp, 'data.frame')
  expect_true(all(
    c(
      "offset",
      "scores",
      "gaba",
      "acetylcholine",
      "glutamate",
      "octopamine",
      "serotonin",
      "dopamine",
      "top.nt"
    ) %in% names(ntp)
  ))
  # top prediction is acetylcholine
  expect_output(print(ntp), "\nacetylcholine")

  skip_if_not_installed('ggplot2')
  expect_is(p <- flywire_ntplot(ntp), 'ggplot')
})
