test_that("dr_fafbseg works", {
  expect_output(dr_fafbseg(), "java", ignore.case=TRUE)
})

test_that("pyids2bit64 works", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  reticulate::py_run_string('import numpy as np')

  sids=c("720575940625861628","720575940621611957")
  sids64=bit64::as.integer64(c("720575940625861628","720575940621611957"))
  expr=sprintf("np.array([%s])", paste(sids, collapse=","))
  pyids=reticulate::py_eval(expr, convert = F)

  expect_equal(pyids2bit64(pyids, as_character = T), sids)
  expect_equal(pyids2bit64(pyids, as_character = F), sids64)
  expect_equal(pyids2bit64(pyids[0], as_character = T), sids[1])
  expect_equal(pyids2bit64(pyids[1], as_character = T), sids[2])
  expect_equal(pyids2bit64(rids2pyint(sids, numpyarray = T), as_character = F),
               sids64)
  expect_equal(pyids2bit64(rids2pyint(sids64, numpyarray = T), as_character = F),
               sids64)
  expect_equal(pyids2bit64(rids2pyint(sids64[1], numpyarray = T), as_character = F),
               sids64[1])
  expect_equal(pyids2bit64(reticulate::py_eval("[9223372036854775807]", convert = F)),
                           "9223372036854775807")
  expect_error(pyids2bit64(
    reticulate::py_eval("np.array([9223372036854775808])", convert = F)),
    "int64 overflow")
})

test_that("pandas2df in-memory conversion preserves int64 columns", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  skip_if_not(reticulate::py_module_available("numpy"))
  skip_if_not(reticulate::py_module_available("pandas"))

  np <- reticulate::import("numpy", convert = FALSE)
  pd <- reticulate::import("pandas", convert = FALSE)
  sids <- c("720575940625861628", "720575940621611957")
  df <- pd$DataFrame(list(
    id = np$array(sids, dtype = "int64"),
    uid = np$array(c("123", "456"), dtype = "uint64"),
    label = c("a", "b"),
    score = c(1.5, 2.5),
    ok = c(TRUE, FALSE)
  ))

  out <- pandas2df(df, use_arrow = FALSE)
  expect_s3_class(out, "data.frame", exact = TRUE)
  expect_true(bit64::is.integer64(out$id))
  expect_true(bit64::is.integer64(out$uid))
  expect_equal(as.character(out$id), sids)
  expect_equal(as.character(out$uid), c("123", "456"))
  expect_identical(out$label, c("a", "b"))
  expect_equal(out$score, c(1.5, 2.5))
  expect_identical(out$ok, c(TRUE, FALSE))
})

test_that("pandas2df in-memory conversion reads pandas Series explicitly", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  skip_if_not(reticulate::py_module_available("numpy"))
  skip_if_not(reticulate::py_module_available("pandas"))

  np <- reticulate::import("numpy", convert = FALSE)
  pd <- reticulate::import("pandas", convert = FALSE)
  sids <- c("720575940625861628", "720575940621611957")
  df <- pd$DataFrame(list(
    id = np$array(sids, dtype = "int64"),
    pt_position = list(c(1L, 2L, 3L), c(4L, 5L, 6L)),
    label = c("a", "b")
  ))
  series <- reticulate::py_get_item(df, "id")
  expect_s3_class(series, "pandas.core.series.Series")
  expect_equal(as.character(series$dtype), "int64")

  out <- pandas2df(df)
  expect_true(bit64::is.integer64(out$id))
  expect_equal(as.character(out$id), sids)
  expect_identical(out$pt_position, list(1:3, 4:6))
  expect_identical(out$label, c("a", "b"))
})

test_that("pandas2df in-memory conversion patches nullable ids and datetimes", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  skip_if_not(reticulate::py_module_available("pandas"))

  reticulate::py_run_string("
import pandas as pd
pdf_obj_ids = pd.DataFrame({
    'id': [6507536],
    'created': [pd.Timestamp('2021-06-23 19:55:36')],
    'pt_root_id': [720575940631797753],
    'pt_supervoxel_id': [80999991094644060],
    'pt_position': [[1, 2, 3]]
})
pdf_obj_ids['pt_root_id'] = pdf_obj_ids['pt_root_id'].astype('Int64')
pdf_obj_ids['pt_supervoxel_id'] = pdf_obj_ids['pt_supervoxel_id'].astype('Int64')
")
  df <- reticulate::py_eval("pdf_obj_ids", convert = FALSE)

  out <- pandas2df(df)
  expect_equal(out$id, 6507536)
  expect_s3_class(out$created, "POSIXct")
  expect_true(bit64::is.integer64(out$pt_root_id))
  expect_true(bit64::is.integer64(out$pt_supervoxel_id))
  expect_equal(as.character(out$pt_root_id), "720575940631797753")
  expect_equal(as.character(out$pt_supervoxel_id), "80999991094644060")
  expect_identical(out$pt_position, list(1:3))
})

test_that("tabify_coords works", {
  m=matrix(1:6, ncol=3, byrow = T)
  expect_equal(tabify_coords(m, FUN=I), c("1\t2\t3", "4\t5\t6"))
})

test_that('module_version works',{
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  expect_true(is.na(module_version('rhubarb')))
})

test_that("pandas2df can keep the pandas index as first column", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  skip_if_not(reticulate::py_module_available("pandas"))

  reticulate::py_run_string('import pandas as pd')
  pdd = reticulate::py_eval(
    paste0(
      "pd.DataFrame(",
      "{'size_nm3': [100, 200]}, ",
      "index=pd.Index([720575940600000004, 720575940600000005], name='l2_id'))"
    ),
    convert = FALSE
  )

  df = pandas2df(pdd, use_arrow = FALSE, keep_index = TRUE, tibble = TRUE)

  expect_equal(names(df)[1], "l2_id")
  expect_equal(as.character(df$l2_id),
               c("720575940600000004", "720575940600000005"))
  expect_equal(df$size_nm3, c(100, 200))
})

test_that("pandas2df keeps legacy data.frame index behaviour by default", {
  skip_if_not_installed('reticulate')
  skip_if_not(reticulate::py_available())
  skip_if_not(reticulate::py_module_available("pandas"))

  reticulate::py_run_string('import pandas as pd')
  pdd = reticulate::py_eval(
    "pd.DataFrame({'x': [10, 20]}, index=pd.Index([101, 102], name='idx'))",
    convert = FALSE
  )

  df = pandas2df(pdd, use_arrow = FALSE)

  expect_s3_class(df, "data.frame", exact = TRUE)
  expect_equal(rownames(df), c("101", "102"))
  expect_equal(names(df), "x")
})

test_that("internet_ok works", {
  skip_if_offline()
  expect_true(internet_ok())
})

test_that("classify_object_values flattens uniform-scalar object columns", {
  # integer-shaped strings -> numeric, including values > 2^31 that would
  # overflow R int32 if naively converted per-cell (the L2 cache bug)
  expect_equal(
    fafbseg:::classify_object_values(c("90040320", "11301120", "3999835136")),
    c(90040320, 11301120, 3999835136)
  )
  # float-shaped strings -> numeric
  expect_equal(
    fafbseg:::classify_object_values(c("0.5", "-1.25", "1e3")),
    c(0.5, -1.25, 1000)
  )
  # NAs preserved
  expect_equal(
    fafbseg:::classify_object_values(c("1", NA, "3")),
    c(1, NA, 3)
  )
  # all-NA -> NA of unspecified type
  expect_true(all(is.na(fafbseg:::classify_object_values(c(NA_character_, NA_character_)))))
})

test_that("classify_object_values returns integer64 for ints beyond 2^53", {
  vals <- c("9007199254740993", "10")  # 2^53 + 1, just past double precision
  out  <- fafbseg:::classify_object_values(vals)
  expect_s3_class(out, "integer64")
  expect_equal(as.character(out), vals)
})

test_that("classify_object_values returns NULL on int64 overflow", {
  # ~10^20 overflows int64; safer to leave the column alone
  expect_null(fafbseg:::classify_object_values(c("123456789012345678901", "1")))
})

test_that("classify_object_values falls through to character on mixed content", {
  expect_equal(
    fafbseg:::classify_object_values(c("foo", "bar")),
    c("foo", "bar")
  )
})
