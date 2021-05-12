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

  nosynapses="720575940425537043"
  # nb there is an extra column when there are multiple input queries
  # and we need to subset both to ensure regtemplate attribute is lost
  expect_equal(flywire_partners(c("720575940616243077", nosynapses))[names(outs)],
               outs[names(outs)])
  both=flywire_partners("720575940616243077", partners = 'both')
  expect_true(all(ins$offset %in% both$offset))
  expect_true(all(outs$offset %in% both$offset))
  expect_true(all(both$offset %in% c(outs$offset, ins$offset)))

  skip_if_not_installed('bit64')
  kcs=bit64::as.integer64(c("720575940609992371","720575940623755722"))
  expect_is(flywire_partners(kcs), 'data.frame')

  expect_warning(flywire_partners(c(kcs[1],kcs[1])), "duplicate")

  top5in = c(
    "720575940625862385",
    "720575940609920691",
    "720575940628437878",
    "720575940620320297",
    "720575940636289469"
  )
  top5out = c(
    "720575940636289469",
    "720575940629952303",
    "720575940622417139",
    "720575940628437878",
    "720575940626114822"
  )
  top3out=top5out[1:3]
  top1out=top5out[1]
  baseline=structure(c(0, 19, 11, 0, 151, 80, 0, 8, 2, 13, 52, 0, 0, 0, 16, 3,
                       24, 160, 0, 8, 20, 19, 6, 34, 0),
                     .Dim = c(5L, 5L), .Dimnames = list(top5in, top5out))
  expect_equal(flywire_adjacency_matrix(inputids = top5in, outputids = top5out,
                                        method = 'auto'),
               baseline)
  expect_equal(flywire_adjacency_matrix(inputids = top5in, outputids = top3out,
                                        method = 'auto'),
               baseline[, top3out])
  expect_equal(flywire_adjacency_matrix(inputids = top5in, outputids = top1out,
                                        method = 'auto'),
               baseline[, top1out, drop=F])

  if(!is.null(flywireids_tbl())) {
    # if we have the table then auto => sqlite, so check spine
    expect_equal(
      flywire_adjacency_matrix(inputids = top5in, outputids = top5out,
        method = 'spine'),
      baseline
    )
  }

  # check for equivalence of sqlite and spine methods if we have sqlite
  skip_if(is.null(synlinks_tbl()), "Skipping tests relying on sqlite databases")

  both.sqlite=flywire_partners("720575940616243077", partners = 'both',
                               details=T, method='sqlite', reference='FlyWire')
  both.spine =flywire_partners("720575940616243077", partners = 'both',
                               details=T, method = 'spine')
  common_cols=intersect(colnames(both.sqlite), colnames(both.spine))
  expect_equal(both.sqlite[common_cols], both.spine[common_cols], tolerance = 1e-5)
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

  expect_is(fw.ann <- flywire_synapse_annotations(x="720575940615237849",sample=10), c("data.frame"))
  expect_equal(nrow(fw.ann), 10)
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
  skip_if(is.null(synlinks_tbl()),
          "Skipping flywire_neurons_add_synapses test as no synlinks sqlite db!")
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if_not_installed('reticulate')
  skip_if(inherits(token, "try-error"),"Skipping live flywire tests")
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")
  expect_is(fwskel <- readRDS(testthat::test_path("testdata/flywire_neuron_skeleton.rds")), 'neuronlist')

  if(!is.null(ntpredictions_tbl())) {
    expect_is(neuron.syn <- flywire_neurons_add_synapses(x=fwskel, transmitters = TRUE, method = "auto"), c("neuronlist"))
    expect_is(preds <- neuron.syn[[1]]$ntpred,'table')
    # check the actual prediction
    expect_named(preds[1], "acetylcholine")
  }else{
    expect_is(neuron.syn <- flywire_neurons_add_synapses(x=fwskel, transmitters = FALSE, method = "spine"), c("neuronlist"))
    expect_named(neuron.syn[[1]]$connectors, c("offset", "prepost", "x", "y", "z", "scores", "cleft_scores",
                                               "segmentid_pre", "segmentid_post", "pre_svid", "post_svid", "pre_id",
                                               "post_id", "top.nt", "treenode_id", "connector_id"))
  }
  expect_is(neuron.syn[,], 'data.frame')
  expect_equal(length(neuron.syn), 1L)
  expect_is(neuron.syn[[1]], c("catmaidneuron"))
  expect_is(neuron.syn[[1]]$connectors, c("data.frame"))
  expect_true(nrow(neuron.syn[[1]]$connectors)>0)
  # check that we get ~the same as flywire_ntpred
  expect_is(fw.id <- flywire_xyz2id(nat::xyzmatrix(neuron.syn[[1]]$d[1,]),rawcoords = FALSE),"character")
  ntp <- try(flywire_ntpred(fw.id), silent = TRUE)
  expect_named(sort(table(ntp$top.nt),decreasing = TRUE)[1], "acetylcholine")
  expect_equal(sort(table(ntp$top.nt)/nrow(ntp),decreasing = TRUE)*100, neuron.syn[[1]]$ntpred)
})
