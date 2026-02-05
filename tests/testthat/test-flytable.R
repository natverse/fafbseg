test_that("query works", {

  ac=try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable tests as unable to login!")

  # sometimes the server seems to give up when requesting a token from each base
  fat <- try(flytable_alltables())
  skip_if(inherits(fat, 'try-error'),
          "skipping flytable tests as having trouble listing all tables!")

  # queries fly table for cell types
  expect_equal(dl4ids <- flywire_ids('DL4_adPN_L', version=630), "720575940627708688")
  expect_true(length(flywire_ids('class:MBON', integer64 = T))>90)

  expect_equal(mbon0x <- flytable_cell_types('MBON0%'),
               flytable_cell_types('/type:MBON0[1-9]', table = 'info'))
  expect_equal(flytable_meta(mbon0x), mbon0x)
  expect_true(length(flywire_ids('super:sensory', integer64 = T))>1000)
  expect_error(flywire_ids('pudding:sensory'))

  tf=tempfile('info.json')
  expect_silent(fct2nginfo(f=tf, ids = 'MBON%', gluestr = "{cell_type}_{toupper(substr(side,1,1))}"))
  expect_true(is.list(l <- read_nginfo(tf)))
  expect_equal(l$inline$ids, flywire_ids('MBON%'))

  expect_s3_class(df <- flytable_query("select fruit_name, person, _ctime, date_wminute FROM testfruit WHERE nid<=3", limit=3L),
                  'data.frame')
  expect_equal(nrow(df), 3L)
  expect_s3_class(fruit <- flytable_list_rows('testfruit'), 'data.frame')
  expect_equal(flytable_list_rows('testfruit', limit=3), fruit[1:3,])
  expect_equal(flytable_list_rows('testfruit', limit=3, chunksize = 2), fruit[1:3,])

  # This test fails sporadically when multiple processes are accessing the db
  # so I think we have to skip
  # expect_equal(flytable_nrow('testfruit'), nrow(fruit))
  # same representation via flytable_list_rows or flytable_query
  expect_equal(fruit[rownames(df),colnames(df)], df)

  expect_true(
    flytable_update_rows(table = 'testfruit',
                         fruit[min(4, nrow(fruit)),
                                   c("_id", "fruit_name", "person", "nid")],
                         chunksize = 1))
  # use a random id to avoid race conditions with other processes
  nid=sample.int(1e7, size = 1, replace = T)
  expect_true(flytable_append_rows(
    table = 'testfruit',
    data.frame(fruit_name='kiwi', person='Frederick the Great', nid=nid)))

  # now delete that row
  qu=glue::glue("SELECT '_id' FROM testfruit WHERE person='Frederick the Great' AND nid={nid}")
  expect_true(nrow(iddf <- flytable_query(qu))>0)

  if(nrow(iddf)>10) {
    Sys.sleep(3)
    flytable_delete_rows(iddf[['_id']], table = 'testfruit', DryRun = F)
  }
  # make a fake neuronlist
  nl=Cell07PNs[seq_along(dl4ids)]
  nl[,]=NULL
  names(nl)=dl4ids
  expect_warning(add_celltype_info(nl, version = 630, suffix = '.y', table = 'info'))

  # check we can get ids from info table
  expect_equal(flywire_ids('LT33', version = 571),
               c("720575940615952450", "720575940634931552"))

  # check handling of unique elements. Lots of duplicates for glia
  expect_warning(
    glialinfou <-
      flytable_meta(
        ids = 'cell_class:putative_glia',
        table = 'info',
        unique = T
      )
  )
  expect_true(all(
    flytable_meta(
      ids = 'cell_class:putative_glia',
      table = 'info',
      unique = F
    )$root_id %in% glialinfou$root_id
  ))

  expect_equal(lt33 <- flywire_ids('LT33', version = 630),
               c("720575940646126190", "720575940627348057"))

  expect_equal(
    withr::with_options(list(fafbseg.use_static_celltypes=T),
                        flywire_ids('LT33', version = 630)),
    lt33)

  expect_equal(
    withr::with_options(list(fafbseg.use_static_celltypes=T),
                        flytable_meta('720575940625808642', version = 630)$side),
    'left')
})



test_that("read only shared tables", {
  # check we can handle situation where user is not a full member of workspace
  # but just has access to a specific shared table
  # user
  ac=fafbseg::flytable_login(token = '22791a98a299312d32539254430ab436bd59a3e7')
  expect_true("info"%in%flytable_alltables(ac)$name)
})


test_that("flytable_cached_table works", {
  ac <- try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable_cached_table tests as unable to login!")

  fat <- try(flytable_alltables())
  skip_if(inherits(fat, 'try-error'),
          "skipping flytable_cached_table tests as having trouble listing all tables!")

  # Clear any existing cache for testfruit
  fc <- fafbseg:::flytable_cache()
  fc$remove('testfruit')

  # Test 1: Basic fetch (cache miss)
  fruit1 <- flytable_cached_table('testfruit')
  expect_s3_class(fruit1, 'data.frame')
  expect_true(nrow(fruit1) > 0)
  expect_true(!is.null(attr(fruit1, 'mtime')))

  # Test 2: Cache hit within expiry window
  fruit2 <- flytable_cached_table('testfruit', expiry = 3600)
  expect_equal(fruit1, fruit2)
  expect_equal(attr(fruit1, 'mtime'), attr(fruit2, 'mtime'))

  # Test 3: Force sync with expiry = 0
  fruit3 <- flytable_cached_table('testfruit', expiry = 0)
  expect_s3_class(fruit3, 'data.frame')
  expect_equal(nrow(fruit3), nrow(fruit1))

  # Test 4: Force complete refresh
  fruit4 <- flytable_cached_table('testfruit', refresh = TRUE)
  expect_s3_class(fruit4, 'data.frame')
  expect_true(!is.null(attr(fruit4, 'mtime')))

  # Test 5: mtime attribute is a valid timestamp
  mtime <- attr(fruit4, 'mtime')
  expect_true(is.character(mtime))
  expect_true(nchar(mtime) > 10)  # Should be a proper timestamp string

  # Cleanup

  fc$remove('testfruit')
})
