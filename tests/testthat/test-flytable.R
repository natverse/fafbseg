test_that("query works", {

  ac=try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable tests as unable to login!")

  expect_s3_class(df <- flytable_query("select flywire_svid, hemibrain_match FROM fafb_hemilineages_survey WHERE hemibrain_match!=''", limit=10),
                  'data.frame')
  expect_equal(nrow(df), 10L)
  expect_s3_class(expect_warning(fruit <- flytable_list_rows('testfruit')), 'data.frame')
  expect_equal(flytable_nrow('testfruit'), nrow(fruit))
  expect_true(
    flytable_update_rows(table = 'testfruit',
                         fruit[c("_id", "fruit_name", "person", "nid")],
                         chunksize = 1))
  expect_true(flytable_append_rows(
    table = 'testfruit',
    data.frame(fruit_name='kiwi', person='Frederick the Great', nid=6)))

  # now delete that row
  expect_true(nrow(iddf <- flytable_query("SELECT '_id' FROM testfruit WHERE person='Frederick the Great'"))>0)
  if(nrow(iddf)>10) {
    Sys.sleep(3)
    tfbase=flytable_base('testfruit')
    pyiddf=reticulate::r_to_py(iddf)
    tfbase$batch_delete_rows(table_name = 'testfruit', row_ids = pyiddf$values$tolist())
  }
})
