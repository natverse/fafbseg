# fake root ids at layer 10
fake_rootids <- function(n) as.character(bit64::as.integer64("720575940600000000") + seq_len(n))

# fake fetcher returning two leaves per id, except the last id which has none
fake_fetcher <- function(env, empty=NULL) {
  function(ids) {
    env$calls[[length(env$calls) + 1]] <- ids
    sapply(ids, function(i) {
      if (i %in% empty) bit64::integer64()
      else bit64::as.integer64(i) * 10 + 0:1
    }, simplify = FALSE)
  }
}

test_that("cave_leaves_cached chunks, caches and preserves order", {
  ids = fake_rootids(5)
  cache = cachem::cache_mem()
  # legacy l2ids cache entries were uncompressed integer64 or character
  cache$set(ids[3], bit64::as.integer64(c("30", "31")))
  cache$set(ids[4], c("40", "41"))
  env = new.env(); env$calls = list()
  fetch = fake_fetcher(env, empty = ids[5])

  x = c(ids, ids[1], "0")
  res = fafbseg:::cave_leaves_cached(x, fetch = fetch, chunksize = 2,
                                     cache = cache, stop_layer = 2L)
  expect_equal(names(res), x)
  # cached and layer 0 ids are not fetched
  expect_equal(env$calls, list(ids[1:2], ids[5]))
  expect_true(all(sapply(res, bit64::is.integer64)))
  expect_equal(res[[ids[2]]], bit64::as.integer64(ids[2]) * 10 + 0:1)
  expect_equal(as.character(res[[ids[3]]]), c("30", "31"))
  expect_equal(as.character(res[[ids[4]]]), c("40", "41"))
  expect_length(res[[ids[5]]], 0)
  expect_length(res[["0"]], 0)
  # new entries are stored compressed; empty results are not cached
  expect_setequal(cache$keys(), ids[1:4])
  expect_type(cache$get(ids[1]), "raw")

  # second call is served from cache apart from the empty result
  env$calls = list()
  res2 = fafbseg:::cave_leaves_cached(x, fetch = fetch, chunksize = 2,
                                      cache = cache, stop_layer = 2L)
  expect_equal(res2, res)
  expect_equal(env$calls, list(ids[5]))

  # chunksize=FALSE fetches one id at a time; no cache fetches everything
  env$calls = list()
  fafbseg:::cave_leaves_cached(ids[1:2], fetch = fetch, chunksize = FALSE)
  expect_equal(env$calls, list(ids[1], ids[2]))
  expect_error(fafbseg:::cave_leaves_cached(ids[1:2], fetch = fetch,
                                            chunksize = 0))
})

test_that("cave_leaves_cached uses custom cache keys", {
  ids = fake_rootids(2)
  cache = cachem::cache_mem()
  env = new.env(); env$calls = list()
  key = function(id) paste0(id, "ooo", "abc")
  fafbseg:::cave_leaves_cached(ids, fetch = fake_fetcher(env), chunksize = 10,
                               cache = cache, key = key)
  expect_setequal(cache$keys(), paste0(ids, "oooabc"))
})

test_that("leaves cache encoding round trips and reads legacy formats", {
  ids = bit64::as.integer64(c("78112261444987077", "720575940623755722"))
  enc = fafbseg:::cave_leaves_encode(ids)
  expect_equal(fafbseg:::cave_leaves_decode(enc), ids)
  bytes = writeBin(unclass(ids), raw())
  zl = memCompress(bytes, type = "gzip")
  expect_true(fafbseg:::is_zlib_header(zl))
  expect_equal(fafbseg:::cave_leaves_decode(zl), ids)
  skip_if_not_installed("brotli")
  br = brotli::brotli_compress(bytes, quality = 2)
  expect_false(fafbseg:::is_zlib_header(br))
  expect_equal(fafbseg:::cave_leaves_decode(br), ids)
  # historical empty entries
  expect_equal(fafbseg:::cave_leaves_decode(raw()), bit64::integer64())
})

test_that("cave_id_layer decodes chunkedgraph layer", {
  expect_equal(fafbseg:::cave_id_layer(c("0", "720575940623755722",
                                         "648518347585700377")),
               c(0L, 10L, 9L))
})

test_that("flywire_l2ids returns vector for one id, list otherwise", {
  skip_if_not_installed("mockery")
  ids = fake_rootids(2)
  env = new.env(); env$calls = list()
  mockery::stub(flywire_l2ids, "flywire_cave_client",
                function(...) list(datastack_name = "test"))
  mockery::stub(flywire_l2ids, "cave_leaves_fetcher",
                function(...) fake_fetcher(env))
  expect_true(bit64::is.integer64(flywire_l2ids(ids[1], cache = FALSE)))
  res = flywire_l2ids(ids, cache = FALSE, integer64 = FALSE)
  expect_equal(names(res), ids)
  expect_type(res[[1]], "character")
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
  expect_identical(batch, single)
  # compare with the original implementation (one get_leaves call per id)
  fcc = flywire_cave_client()
  orig = lapply(rids, function(rid) fafbseg:::pyids2bit64(
    reticulate::py_call(fcc$chunkedgraph$get_leaves, rid, stop_layer = 2L),
    as_character = FALSE))
  expect_identical(unname(batch), orig)

  # a chunk containing a single id must still be sent as a list
  expect_equal(sort(flywire_l2ids(rids[1], cache = FALSE)), sort(single[[1]]))
  # invalid ids give empty integer64 vectors
  res = flywire_l2ids(c(rids[1], "1"), cache = FALSE)
  expect_true(bit64::is.integer64(res[["1"]]))
  expect_length(res[["1"]], 0)
})

test_that("flywire_leaves chooses CAVE or CloudVolume path", {
  skip_if_not_installed("mockery")
  ids = fake_rootids(3)
  env = new.env(); env$calls = list(); env$cv = character()
  cache = cachem::cache_mem()
  mockery::stub(flywire_leaves, "ngl_segments", function(x, ...) x)
  mockery::stub(flywire_leaves, "flywire_cloudvolume_url",
                function(...) "graphene://https://example.org/table/test")
  mockery::stub(flywire_leaves, "flywire_leaves_cache", function(...) cache)
  mockery::stub(flywire_leaves, "flywire_leaves_cave_client",
                function() list(datastack_name = "test"))
  mockery::stub(flywire_leaves, "cave_leaves_fetcher",
                function(...) fake_fetcher(env))
  mockery::stub(flywire_leaves, "flywire_leaves_impl", function(x, ...) {
    env$cv = c(env$cv, x)
    bit64::as.integer64(x) * 10 + 0:1
  })

  res = flywire_leaves(ids[1:2], integer64 = TRUE)
  expect_equal(env$calls, list(ids[1:2]))
  expect_length(env$cv, 0)
  expect_equal(res[[ids[2]]], bit64::as.integer64(ids[2]) * 10 + 0:1)
  urlhash = digest::digest("graphene://https://example.org/table/test",
                           algo = "xxhash64")
  expect_setequal(cache$keys(), paste0(ids[1:2], "ooo", urlhash))

  # chunksize=FALSE uses CloudVolume one id at a time, sharing the cache
  expect_type(flywire_leaves(ids, chunksize = FALSE), "list")
  expect_equal(env$cv, ids[3])
  # so does an explicit cloudvolume.url
  env$cv = character()
  flywire_leaves(ids, cache = FALSE, cloudvolume.url = "graphene://https://x/y")
  expect_equal(env$cv, ids)
  expect_equal(length(env$calls), 1)
  expect_type(flywire_leaves(ids[1]), "character")
})

test_that("flywire_leaves CAVE and CloudVolume paths agree", {
  skip_if_not_installed('reticulate')
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"), "Skipping live flywire tests")
  skip_if_not(reticulate::py_module_available("caveclient"),
              "Skipping live flywire tests requiring python caveclient module")
  skip_if(is.null(fafbseg:::flywire_leaves_cave_client()),
          "CAVE client does not match default segmentation")
  rids = try(flywire_rootid(c('81700174112186909', '78112261444987077')),
             silent = TRUE)
  skip_if(inherits(rids, "try-error"), "Skipping: unable to resolve root ids")
  # include a stale root id with a known number of supervoxels
  rids = c(rids, "720575940623755722")
  cave = flywire_leaves(rids, cache = FALSE, integer64 = TRUE)
  cv = flywire_leaves(rids, cache = FALSE, integer64 = TRUE, chunksize = FALSE)
  expect_named(cave, rids)
  expect_length(cave[[3]], 8536)
  # identical including order since e.g. flywire_latestid samples leaves
  expect_identical(cave, cv)
  # and round trip through the cache
  cache = cachem::cache_mem()
  mockery::stub(flywire_leaves, "flywire_leaves_cache", function(...) cache)
  expect_identical(flywire_leaves(rids, integer64 = TRUE), cv)
  expect_identical(flywire_leaves(rids, integer64 = TRUE), cv)
  expect_length(cache$keys(), 3)
})

test_that("leaves cache decoding falls back between formats", {
  ids = bit64::as.integer64(c("78112261444987077", "720575940623755722"))
  bytes = writeBin(unclass(ids), raw())
  zl = memCompress(bytes, type = "gzip")
  # explicit legacy types
  expect_equal(fafbseg:::flywire_leaves_frombytes(memCompress(bytes, "xz"), type = "xz"), bytes)
  expect_equal(fafbseg:::flywire_leaves_frombytes(bytes, type = "none"), bytes)
  # zlib data mislabelled as brotli falls back to zlib
  expect_equal(fafbseg:::flywire_leaves_frombytes(zl, type = "brotli"), bytes)
  skip_if_not_installed("brotli")
  br = brotli::brotli_compress(bytes, quality = 2)
  # brotli data mislabelled as zlib falls back to brotli
  expect_equal(fafbseg:::flywire_leaves_frombytes(br, type = "gzip"), bytes)
  # garbage fails rather than returning something
  expect_error(fafbseg:::flywire_leaves_frombytes(as.raw(1:20)))
})

test_that("leaves cache encoding falls back to zlib without brotli", {
  skip_if_not_installed("mockery")
  ids = bit64::as.integer64(c("78112261444987077", "720575940623755722"))
  enc = fafbseg:::cave_leaves_encode
  mockery::stub(enc, "requireNamespace", function(...) FALSE)
  zl = enc(ids)
  expect_true(fafbseg:::is_zlib_header(zl))
  expect_equal(fafbseg:::cave_leaves_decode(zl), ids)
})

test_that("flywire_leaves refuses cache with non-standard bbox", {
  expect_error(flywire_leaves("720575940623755722", bbox = matrix(0, 2, 3)),
               "bounding box")
})
