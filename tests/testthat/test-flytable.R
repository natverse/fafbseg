test_that("query works", {

  ac=try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable tests as unable to login!")

  # sometimes the server seems to give up when requesting a token from each base
  fat <- try(flytable_alltables())
  skip_if(inherits(fat, 'try-error'),
          "skipping flytable tests as having trouble listing all tables!")

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
})
