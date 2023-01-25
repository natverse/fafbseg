test_that("nginfo works", {
  df=data.frame(id=c(10000,10002), type=c("DNp01"), side=c("L", "R"))
  tf=tempfile(pattern = 'mancinfo.json')
  on.exit(unlink(tf))
  expect_silent(write_nginfo(df, f = tf))
  bl = list(`@type` = "neuroglancer_segment_properties",
            inline = list(
              ids = c("10000", "10002"),
              properties = list(list(
                id = "label",
                type = "label",
                values = c("DNp01_L", "DNp01_R")
              ))
            ))
  expect_equal(read_nginfo(tf), bl)
  expect_equal(read_nginfo(tf), nginfo(df))
})
