test_that("flywire connectome data dumps work", {
  fcddir <- file.path(system.file('tests/testthat/testdata/fcd', package = 'fafbseg'))
  op <- options(fafbseg.flywire_connectome_dir=fcddir)
  on.exit(options(op))
  fcd=try(flywire_connectome_data('syn', version=447), silent = TRUE)

  skip_if(inherits(fcd, 'try-error'),
          message = 'Skipping tests of flywire connectome data since dump 447 unavailable!')

  expect_is(suppressWarnings(flywire_connectome_data_version()), 'integer')
  expect_true(flywire_connectome_data_version()>400)

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

  res_out <- flywire_partner_summary2(da2ids, partners = 'out', version=447,
                                     add_cell_types = F, threshold = 35, by.roi = T, summarise = F)
  try({
    message('=== DIAGNOSTIC: flywire_partner_summary2 OUT ===')
    message('--- sessionInfo ---')
    print(sessionInfo())
    pkgs <- c('arrow','dplyr','tibble','vctrs','bit64','digest','jsonlite')
    for (p in pkgs) message(p, ':', if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else 'NOT INSTALLED')
    if (requireNamespace('arrow', quietly = TRUE)) {
      try(print(arrow::arrow_info()), silent = TRUE)
    }
    message('class(res_out): ', paste(class(res_out), collapse = ', '))
    message('names(res_out): ', paste(names(res_out), collapse = ', '))
    message('head(as.data.frame(res_out)):')
    print(utils::head(as.data.frame(res_out), 50))
    ct_out <- sapply(res_out, function(x) paste(class(x), collapse = '/'))
    message('column classes:'); print(ct_out)
    if (requireNamespace('digest', quietly = TRUE)) {
      message('digest(xxhash32) on res_out: ', tryCatch(digest::digest(res_out, algo = 'xxhash32'), error = function(e) paste('error:', e$message)))
      message('digest(JSON sha256) on as.data.frame(res_out): ', tryCatch(digest::digest(jsonlite::toJSON(as.data.frame(res_out), auto_unbox = TRUE, null = 'null', digits = 22), algo = 'sha256'), error = function(e) paste('error:', e$message)))
    }
  }, silent = TRUE)

  expect_known_hash(res_out, hash = "36d56aa000")
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = T, summarise = T), hash = 'a8dbcb6031')
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = T), hash = "bd2022cb7c")
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'out', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = F), hash = "361dabe046")

  res_in <- flywire_partner_summary2(da2ids, partners = 'in', version=447,
                                    add_cell_types = F, threshold = 15, by.roi = T, summarise = F)
  try({
    message('=== DIAGNOSTIC: flywire_partner_summary2 IN ===')
    message('--- sessionInfo ---')
    print(sessionInfo())
    pkgs <- c('arrow','dplyr','tibble','vctrs','bit64','digest','jsonlite')
    for (p in pkgs) message(p, ':', if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else 'NOT INSTALLED')
    if (requireNamespace('arrow', quietly = TRUE)) {
      try(print(arrow::arrow_info()), silent = TRUE)
    }
    message('class(res_in): ', paste(class(res_in), collapse = ', '))
    message('names(res_in): ', paste(names(res_in), collapse = ', '))
    message('head(as.data.frame(res_in)):')
    print(utils::head(as.data.frame(res_in), 50))
    ct_in <- sapply(res_in, function(x) paste(class(x), collapse = '/'))
    message('column classes:'); print(ct_in)
    if (requireNamespace('digest', quietly = TRUE)) {
      message('digest(xxhash32) on res_in: ', tryCatch(digest::digest(res_in, algo = 'xxhash32'), error = function(e) paste('error:', e$message)))
      message('digest(JSON sha256) on as.data.frame(res_in): ', tryCatch(digest::digest(jsonlite::toJSON(as.data.frame(res_in), auto_unbox = TRUE, null = 'null', digits = 22), algo = 'sha256'), error = function(e) paste('error:', e$message)))
    }
  }, silent = TRUE)

  expect_known_hash(res_in, hash = "7d1c42c609")
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 35, by.roi = T, summarise = T), hash = '005cd1c504')
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 35, by.roi = F, summarise = T), hash = '2bf5f8f1eb')
  expect_known_hash(flywire_partner_summary2(da2ids, partners = 'in', version=447, add_cell_types = F, threshold = 15, by.roi = F, summarise = F), hash = '1b79889f5f')

})

test_that("flywire connectome data 783 works", {
  flywire_connectome_data_version(set = 783)
  download_flywire_release_data(version = flywire_connectome_data_version())
  on.exit(flywire_connectome_data_version(set = NA))
  op <- options(fafbseg.use_static_celltypes = T)
  on.exit(options(op), add = T)

  skip_if_not_installed('git2r')
  dl4df <- data.frame(
    root_id = c("720575940617343316", "720575940627708688"),
    supervoxel_id = c("80435185581291588", "78957304515029923"),
    side = c("right", "left"), flow = c("intrinsic", "intrinsic"),
    super_class = c("central", "central"),
    cell_class = c("ALPN", "ALPN"),
    cell_sub_class = c("uniglomerular", "uniglomerular"),
    cell_type = c("DL4_adPN", "DL4_adPN"),
    top_nt = c("acetylcholine", "acetylcholine"),
    ito_lee_hemilineage = c("ALad1__prim", "ALad1__prim"),
    hemibrain_type = c("DL4_adPN", "DL4_adPN"),
    fbbt_id = c("FBbt_00100382", "FBbt_00100382"))
  expect_equal(flytable_meta("DL4.*"), dl4df)

  syn=try(flywire_connectome_data('syn', version=783), silent = TRUE)

  skip_if(inherits(syn, 'try-error'),
          message = 'Skipping tests of flywire connectome data since dump 783 unavailable!')

  expect_s3_class(odf <- flywire_partner_summary2(dl4df, partners = 'o', threshold = 20),
                  "data.frame")
  expect_equal(odf$cell_type[1:3], c("LHAV1a1", "LHAV6b4", "LHAV1a1"))

  expect_error(flywire_connectome_data('pre', version='783.2'))
  expect_true(is.object(syn2 <- flywire_connectome_data(version='783.2')))
  expect_s3_class(odf2 <- flywire_partner_summary2('DNa02', partners = 'o', threshold = 60, version=783.2),
                  "data.frame")
  expect_true(nrow(odf2)==10)
})