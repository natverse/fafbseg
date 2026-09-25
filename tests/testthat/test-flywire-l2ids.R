test_that("flywire_l2ids batches uncached ids and uses cache", {
  skip_if_not_installed('mockery')
  cache = cachem::cache_mem()
  cache$set("3", bit64::as.integer64(c("30", "31")))
  calls = list()
  mockery::stub(flywire_l2ids, "flywire_cave_client",
                function(...) list(datastack_name = "test"))
  mockery::stub(flywire_l2ids, "flywire_ids", function(x, ...) as.character(x))
  mockery::stub(flywire_l2ids, "flywire_leaves_cache", function(...) cache)
  mockery::stub(flywire_l2ids, "flywire_l2ids_many", function(x, fcc) {
    calls[[length(calls) + 1]] <<- x
    sapply(x, function(i) bit64::as.integer64(paste0(i, c("0", "1"))),
           simplify = FALSE)
  })

  x = c("1", "2", "3", "4", "5", "2")
  res = flywire_l2ids(x, chunksize = 2)
  expect_equal(names(res), x)
  expect_equal(calls, list(c("1", "2"), c("4", "5")))
  expect_true(bit64::is.integer64(res[["1"]]))
  expect_equal(as.character(res[["3"]]), c("30", "31"))
  expect_equal(as.character(res[["5"]]), c("50", "51"))
  expect_setequal(cache$keys(), as.character(1:5))

  # all cached now, so no further calls
  calls = list()
  expect_equal(flywire_l2ids("4", integer64 = FALSE), c("40", "41"))
  expect_length(calls, 0)

  # cache=FALSE forces a fetch
  flywire_l2ids("4", cache = FALSE)
  expect_equal(calls, list("4"))
})

test_that("flywire_l2ids chunksize=FALSE fetches one id at a time and caches", {
  skip_if_not_installed('mockery')
  cache = cachem::cache_mem()
  cache$set("3", bit64::as.integer64(c("30", "31")))
  calls = list()
  mockery::stub(flywire_l2ids, "flywire_cave_client",
                function(...) list(datastack_name = "test"))
  mockery::stub(flywire_l2ids, "flywire_ids", function(x, ...) as.character(x))
  mockery::stub(flywire_l2ids, "flywire_leaves_cache", function(...) cache)
  mockery::stub(flywire_l2ids, "flywire_l2ids_many",
                function(...) stop("should not batch"))
  mockery::stub(flywire_l2ids, "reticulate::py_call", function(f, id, ...) {
    calls[[length(calls) + 1]] <<- id
    paste0(id, c("0", "1"))
  })
  mockery::stub(flywire_l2ids, "pyids2bit64",
                function(x, ...) bit64::as.integer64(x))

  x = c("1", "2", "3", "2")
  res = flywire_l2ids(x, chunksize = FALSE)
  expect_equal(names(res), x)
  expect_equal(calls, list("1", "2"))
  expect_equal(as.character(res[["2"]]), c("20", "21"))
  expect_equal(as.character(res[["3"]]), c("30", "31"))
  expect_setequal(cache$keys(), as.character(1:3))

  calls = list()
  expect_equal(flywire_l2ids("1", chunksize = FALSE, integer64 = FALSE),
               c("10", "11"))
  expect_length(calls, 0)

  expect_error(flywire_l2ids("9", chunksize = 0))
})

test_that("flywire_l2ids batched and single lookups agree", {
  skip_if_not_installed('reticulate')
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  skip_if_not(reticulate::py_module_available("caveclient"),
              "Skipping live flywire tests requiring python caveclient module")
  rids = try(flywire_rootid(c('81700174112186909', '78112261444987077')),
             silent = TRUE)
  skip_if(inherits(rids, "try-error"), "Skipping: unable to resolve root ids")

  batch = flywire_l2ids(rids, cache = FALSE)
  expect_equal(names(batch), rids)
  single = flywire_l2ids(rids, cache = FALSE, chunksize = FALSE)
  expect_equal(lapply(batch, sort), lapply(single, sort))

  # a chunk containing a single id must still be sent as a list
  expect_equal(sort(flywire_l2ids(rids[1], cache = FALSE)), sort(single[[1]]))
  # invalid ids give empty integer64 vectors
  res = flywire_l2ids(c(rids[1], "1"), cache = FALSE)
  expect_true(bit64::is.integer64(res[["1"]]))
  expect_length(res[["1"]], 0)
})
