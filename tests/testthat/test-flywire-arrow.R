test_that("flywire connectome data dumps work", {
  fcddir <- file.path(system.file('tests/testthat/testdata/fcd', package = 'fafbseg'))
  op <- options(fafbseg.flywire_connectome_dir=fcddir)
  on.exit(options(op))
  fcd=try(flywire_connectome_data('syn', version=447), silent = TRUE)

  skip_if(inherits(fcd, 'try-error'),
          message = 'Skipping tests of flywire connectome data since dump 447 unavailable!')

  expect_is(flywire_connectome_data_version(), 'integer')
  expect_true(flywire_connectome_data_version()>400)

  # da2ids=flywire_ids('DA2_lPN', version=447)
  da2ids=c("720575940639337461", "720575940628259407", "720575940611849187",
           "720575940622364184", "720575940624106442", "720575940622734835",
           "720575940622762995", "720575940626937617", "720575940622311704",
           "720575940610052266", "720575940638719104")

  expect_silent(fam <- flywire_adjacency_matrix2(da2ids, version=447, sparse = T))
  expect_is(fam, 'Matrix')
  bl=c(`720575940639337461` = 27, `720575940628259407` = 17, `720575940611849187` = 14,
       `720575940622364184` = 9, `720575940624106442` = 15, `720575940622734835` = 10,
       `720575940622762995` = 34, `720575940626937617` = 10, `720575940622311704` = 10,
       `720575940610052266` = 18, `720575940638719104` = 16)
  expect_equal(rowSums(flywire_adjacency_matrix2(da2ids, version=447, sparse = F)),
               bl)

  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = T, summarise = F), hash = "36d56aa000")
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = T, summarise = T), hash = 'a49ca4b21a')
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = T), hash = "bd2022cb7c")
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = F), hash = "361dabe046")

  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 15, by.roi = T, summarise = F), hash = "7d1c42c609")
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 35, by.roi = T, summarise = T), hash = '620a7be9aa')
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = T), hash = '2bf5f8f1eb')
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 15, by.roi = F, summarise = F), hash = '1b79889f5f')

})
