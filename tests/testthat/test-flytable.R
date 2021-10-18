test_that("query works", {

  ac=try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable tests as unable to login!")

  expect_s3_class(df <- flytable_query("select FLYWIREsvid, hemibrain_match FROM fafb_hemilineages_survey WHERE hemibrain_match!=''", limit=10),
                  'data.frame')
  expect_equal(nrow(df), 10L)
  expect_s3_class(fruit <- flytable_list_rows('testfruit'), 'data.frame')
  expect_true(
    flytable_update_rows(table = 'testfruit',
                         fruit[c("_id", "fruit_name", "person", "nid")],
                         chunksize = 1))
})
