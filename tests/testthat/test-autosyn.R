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

  # check for equivalence of sqlite and spine methods if we have sqlite
  skip_if(is.null(synlinks_tbl()), "Skipping tests relying on sqlite databases")
  if(!is.null(synlinks_tbl())) {
    both.spine=flywire_partners("720575940616243077", partners = 'both', method = 'spine')
    both.details=flywire_partners("720575940616243077", partners = 'both', details=T)
    expect_equal(both.details[colnames(both.spine)], both.spine)
  }

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

test_that("flywire_neurons_add_synapses works", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if_not_installed('reticulate')
  skip_if(inherits(token, "try-error"),"Skipping live flywire tests")
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")
  expect_is(neuron <- readRDS("testdata/flywire_neuron_skeleton.rds"), 'neuronlist')
  skip_if(is.null(ntpredictions_tbl()), "Skipping tests relying on sqlite databases")
  if(!is.null(ntpredictions_tbl())) {
    expect_is(neuron.syn = flywire_neurons_add_synapses(x=neuron, transmitters = TRUE, method = "auto"), c("neuronlist"))
    expect_is(neuron.syn[[1]]$transmitter.predictions,'table')
  }else{
    expect_is(neuron.syn = flywire_neurons_add_synapses(x=neuron, transmitters = FALSE, method = "spine"), c("neuronlist"))
    expect_named(neuron.syn[[1]]$connectors, c("offset", "prepost", "x", "y", "z", "scores", "cleft_scores",
                                               "segmentid_pre", "segmentid_post", "pre_svid", "post_svid", "pre_id",
                                               "post_id", "top.nt", "treenode_id", "connector_id"))
  }
  expect_is(neuron.syn[,], 'data.frame')
  expect_equal(length(neuron.syn), 1L)
  expect_is(neuron.syn[[1]], c("catmaidneuron"))
  expect_is(neuron.syn[[1]]$connectors, c("data.frame"))
  expect_true(nrow(neuron.syn[[1]]$connectors)>0)
})
