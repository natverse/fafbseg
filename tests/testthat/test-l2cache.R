test_that("l2cache_data_to_tibble preserves ids and expands columns", {
  x = list(
    "720575940600000001" = list(
      size_nm3 = 10,
      rep_coord_nm = c(1, 2, 3),
      pca = matrix(1:9, nrow = 3, byrow = TRUE),
      pca_val = c(4, 5, 6),
      chunk_intersect_count = matrix(11:16, nrow = 2, byrow = TRUE)
    ),
    "720575940600000002" = list(
      size_nm3 = 20,
      rep_coord_nm = NA,
      pca = NA,
      pca_val = NA,
      chunk_intersect_count = NA
    )
  )

  res = fafbseg:::l2cache_data_to_tibble(x, split_columns = TRUE)

  expect_equal(
    as.character(res$l2_id),
    c("720575940600000001", "720575940600000002")
  )
  expect_equal(res$size_nm3, c(10, 20))
  expect_equal(res$rep_coord_nm_x, c(1, NA))
  expect_equal(res$rep_coord_nm_y, c(2, NA))
  expect_equal(res$rep_coord_nm_z, c(3, NA))
  expect_equal(
    unname(as.numeric(res[1, c(
      "pca_0_x", "pca_0_y", "pca_0_z",
      "pca_1_x", "pca_1_y", "pca_1_z",
      "pca_2_x", "pca_2_y", "pca_2_z"
    )])),
    1:9
  )
  expect_equal(
    unname(as.numeric(res[1, c(
      "chunk_intersect_count_x_bottom",
      "chunk_intersect_count_y_bottom",
      "chunk_intersect_count_z_bottom",
      "chunk_intersect_count_x_top",
      "chunk_intersect_count_y_top",
      "chunk_intersect_count_z_top"
    )])),
    11:16
  )
})

test_that("l2cache_data_to_tibble can retain list columns", {
  x = list(
    "720575940600000003" = list(
      size_nm3 = 30,
      rep_coord_nm = c(7, 8, 9)
    )
  )

  res = fafbseg:::l2cache_data_to_tibble(x, split_columns = FALSE)

  expect_equal(as.character(res$l2_id), "720575940600000003")
  expect_equal(res$size_nm3, 30)
  expect_equal(res$rep_coord_nm[[1]], c(7, 8, 9))
})

test_that("flywire_l2attributes passes requested datastack to flywire_l2ids", {
  seen = NULL

  mockery::stub(flywire_l2attributes, "flywire_ids", function(x, integer64 = FALSE) x)
  mockery::stub(flywire_l2attributes, "flywire_l2ids", function(x, integer64 = TRUE, datastack_name = NULL, ...) {
    seen <<- datastack_name
    stop("done")
  })

  expect_error(
    flywire_l2attributes(rootid = "720575940600000001", datastack_name = "flywire_fafb_public"),
    "done"
  )
  expect_equal(seen, "flywire_fafb_public")
})
