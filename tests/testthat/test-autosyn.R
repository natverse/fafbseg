test_that("flywire_partners / flywire_partner_summary works", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if_not_installed('reticulate')
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")

  expect_message(df <- flywire_partner_summary("720575940616243077", Verbose = T),
                'Fetching supervoxel.*720575940616243077')
  expect_is(df, 'data.frame')
  expect_named(df, c("post_id", "weight", "n"))
  expect_message(df2 <- flywire_partner_summary("720575940616243077", partners = 'input', Verbose = T),
                 'Fetching supervoxel.*720575940616243077')

  expect_is(ins <- flywire_partners("720575940616243077", partners = 'inputs'), 'data.frame')
  expect_is(outs <- flywire_partners("720575940616243077", partners = 'outputs'), 'data.frame')

  expect_equal(nrow(ins), 156L)
  expect_equal(nrow(outs), 463L)
  both=flywire_partners("720575940616243077", partners = 'both')
  expect_true(all(ins$offset %in% both$offset))
  expect_true(all(outs$offset %in% both$offset))
  expect_true(all(both$offset %in% c(outs$offset, ins$offset)))

  skip_if_not_installed('bit64')
  kcs=bit64::as.integer64(c("720575940609992371","720575940623755722"))
  expect_is(flywire_partners(kcs), 'data.frame')
})

test_that("flywire_ntpred+flywire_ntplot works", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")

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

  kcs=bit64::as.integer64(c("720575940609992371","720575940623755722"))
  ntp2 <-flywire_ntpred(kcs)
})


test_that("fafbseg.sqlitepath is respected",{
  td=tempfile('fakedb')
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))
  tf=file.path(td, "test.db")
  writeLines("DUMMY",  tf)
  withr::with_options(list('fafbseg.sqlitepath'=td),
                      expect_equal(local_or_google("test.db"), tf))
})
