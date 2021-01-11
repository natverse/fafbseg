test_that("flywire_partner_summary works", {
  expect_is(df <- flywire_partner_summary("720575940615237849"), 'data.frame')
  expect_named(df, c("post_id", "weight", "n"))
})

test_that("flywire_ntpred+flywire_ntplot works", {
  ntp <- try(flywire_ntpred("720575940615237849"), silent = TRUE)
  skip_if(inherits(ntp, 'try-error'),
          'No neurotransmitter prediction data available')
  expect_is(ntp, 'data.frame')
  skip_if_not_installed('ggplot2')
  expect_is(p <- flywire_ntplot(ntp), 'ggplot')
  expect_named(df, c("post_id", "weight", "n"))
})
