test_that("key_point_from_neuron picks the principal branch point", {
  # synthetic Y-shaped neuron: single branch point at (0, 10, 0)
  swc <- data.frame(
    PointNo = 1:7,
    Label = c(1, 3, 3, 3, 6, 3, 6),
    X = c(0, 0, 0, 5, 10, -5, -10),
    Y = c(0, 5, 10, 15, 20, 15, 20),
    Z = 0,
    W = 1,
    Parent = c(-1, 1, 2, 3, 4, 3, 6))
  n <- nat::as.neuron(swc)

  pt <- key_point_from_neuron(n, reroot = TRUE)
  expect_length(as.numeric(pt), 3L)
  expect_true(all(is.finite(pt)))
  expect_equal(as.numeric(pt), c(0, 10, 0))

  # deterministic for the same input
  expect_equal(key_point_from_neuron(n, reroot = TRUE), pt)

  # chosen point must lie within the bounding box of the input neuron
  bb <- apply(nat::xyzmatrix(n), 2, range)
  pt3 <- as.numeric(pt)
  expect_true(all(pt3 >= bb[1, ] & pt3 <= bb[2, ]))
})

test_that("key_point_from_neuron falls back to root without a branch point", {
  # an unbranched neuron has no branch point -> falls back to root, with warning
  swc <- data.frame(
    PointNo = 1:3, Label = c(1, 3, 3),
    X = 0, Y = c(0, 5, 10), Z = 0, W = 1, Parent = c(-1, 1, 2))
  n <- nat::as.neuron(swc)
  expect_warning(pt <- key_point_from_neuron(n, reroot = FALSE),
                 "falling back to root")
  expect_length(as.numeric(pt), 3L)
  expect_true(all(is.finite(pt)))
})

test_that("flywire_key_point works on live Kenyon cells", {
  skip_if_not_installed('reticulate')
  token = try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"), "Skipping live flywire_key_point test")
  skip_if_not(reticulate::py_module_available("fafbseg"),
              "skipping live flywire_key_point test as python fafbseg unavailable")
  skip_if_flywire_materialize_unavailable(
    "skipping live flywire_key_point test as flywire materialization unavailable")

  # pin by stable supervoxel ids (KCs, from test-fafbseg-py.R), resolve to
  # current root ids at test time as root ids drift with proofreading
  kcsvids = c("78603674556915608", "78462662124123765", "77547662357982001")
  kcids = try(flywire_rootid(kcsvids), silent = TRUE)
  skip_if(inherits(kcids, "try-error"), "Skipping live flywire_key_point test")

  # single id, raw voxel space (default): 1 x 3 matrix
  expect_true(is.matrix(pt1 <- flywire_key_point(kcids[1])))
  expect_equal(dim(pt1), c(1L, 3L))
  expect_true(all(is.finite(pt1)))

  # single id, nm: length-3 vector
  expect_length(as.numeric(flywire_key_point(kcids[1], raw = FALSE)), 3L)

  # multiple ids: N x 3 matrix, one row per id
  expect_true(is.matrix(pts <- flywire_key_point(kcids)))
  expect_equal(dim(pts), c(length(kcids), 3L))
  expect_true(all(is.finite(pts)))
})
