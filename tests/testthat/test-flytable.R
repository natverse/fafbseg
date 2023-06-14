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

  expect_equal(flytable_nrow('testfruit'), nrow(fruit))
  # same representation via flytable_list_rows or flytable_query
  expect_equal(fruit[rownames(df),colnames(df)], df)

  expect_true(
    flytable_update_rows(table = 'testfruit',
                         fruit[min(4, nrow(fruit)),
                                   c("_id", "fruit_name", "person", "nid")],
                         chunksize = 1))
  expect_true(flytable_append_rows(
    table = 'testfruit',
    data.frame(fruit_name='kiwi', person='Frederick the Great', nid=6)))

  # now delete that row
  expect_true(nrow(iddf <- flytable_query("SELECT '_id' FROM testfruit WHERE person='Frederick the Great'"))>0)

  if(nrow(iddf)>10) {
    Sys.sleep(3)
    flytable_delete_rows(iddf[['_id']], table = 'testfruit')
  }
  # make a fake neuronlist
  nl=Cell07PNs[seq_along(dl4ids)]
  nl[,]=NULL
  names(nl)=dl4ids
  expect_warning(add_celltype_info(nl, version = 630, suffix = '.y', table = 'info'))

  # check we can get ids from info table
  expect_equal(flywire_ids('LT33', version = 571),
               c("720575940615952450", "720575940634931552"))
})

