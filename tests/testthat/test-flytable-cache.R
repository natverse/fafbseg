# Offline tests of flytable_cached_table() cache bookkeeping, with the flytable
# server calls mocked out.

ts <- function(x) format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%S+0000")

# a memory cache that logs the keys written to it
logging_cache <- function() {
  m <- cachem::cache_mem()
  log <- character()
  list(get = m$get, remove = m$remove,
       set = function(key, value) {
         log <<- c(log, key)
         m$set(key, value)
       },
       log = function() log, reset = function() log <<- character())
}

fruit_table <- function(mtime) {
  df <- data.frame(`_id` = c("a", "b"), fruit = c("apple", "banana"),
                   check.names = FALSE)
  attr(df, "mtime") <- mtime
  df
}

local_flytable_mocks <- function(fc, meta, modrows = NULL,
                                 env = parent.frame()) {
  local_mocked_bindings(
    flytable_cache = function() fc,
    flytable_cache_key = function(table, base = NULL) "fruit",
    flytable_full_fetch = function(...) fruit_table(ts("2026-01-01 00:00:00")),
    flytable_sync_metadata = function(table) meta,
    flytable_query = function(...) modrows,
    .env = env
  )
}

test_that("flytable_cached_table stores sync time in its own cache entry", {
  fc <- logging_cache()
  local_flytable_mocks(fc, meta = NULL)

  res <- flytable_cached_table("fruit")
  expect_equal(fc$log(), c("fruit", "fruitmtime"))
  expect_equal(attr(res, "mtime"), ts("2026-01-01 00:00:00"))
  expect_equal(fc$get("fruitmtime"), ts("2026-01-01 00:00:00"))

  # refresh clears both entries and refetches
  fc$set("fruitmtime", ts("2026-02-01 00:00:00"))
  fc$reset()
  res <- flytable_cached_table("fruit", refresh = TRUE)
  expect_equal(fc$log(), c("fruit", "fruitmtime"))
  expect_equal(attr(res, "mtime"), ts("2026-01-01 00:00:00"))
})

test_that("no-op delta sync only advances the sync time", {
  fc <- logging_cache()
  fc$set("fruit", fruit_table(ts("2026-01-01 00:00:00")))
  fc$reset()
  # server unchanged since our last sync
  local_flytable_mocks(fc, meta = list(now = ts("2026-01-02 00:00:00"),
                                       nrow = 2L,
                                       max_mtime = ts("2025-12-31 00:00:00")))

  res <- flytable_cached_table("fruit", expiry = 0)
  expect_equal(fc$log(), "fruitmtime")
  expect_equal(attr(res, "mtime"), ts("2026-01-02 00:00:00"))
  expect_equal(fc$get("fruitmtime"), ts("2026-01-02 00:00:00"))
  expect_equal(nrow(res), 2L)
  # stored table (and its legacy attribute) left alone
  expect_equal(attr(fc$get("fruit"), "mtime"), ts("2026-01-01 00:00:00"))

  # the sidecar sync time now takes precedence over the stored attribute
  fc$reset()
  res2 <- flytable_cached_table("fruit", expiry = 1e9)
  expect_length(fc$log(), 0)
  expect_equal(attr(res2, "mtime"), ts("2026-01-02 00:00:00"))
})

test_that("delta sync with changed rows rewrites table then sync time", {
  fc <- logging_cache()
  fc$set("fruit", fruit_table(ts("2026-01-01 00:00:00")))
  fc$set("fruitmtime", ts("2026-01-01 00:00:00"))
  fc$reset()
  newrow <- data.frame(`_id` = "c", fruit = "cherry", check.names = FALSE)
  local_flytable_mocks(fc, meta = list(now = ts("2026-01-02 00:00:00"),
                                       nrow = 3L,
                                       max_mtime = ts("2026-01-01 12:00:00")),
                       modrows = newrow)

  res <- flytable_cached_table("fruit", expiry = 0)
  expect_equal(fc$log(), c("fruit", "fruitmtime"))
  expect_equal(res$fruit, c("apple", "banana", "cherry"))
  expect_equal(nrow(fc$get("fruit")), 3L)
  expect_equal(attr(res, "mtime"), ts("2026-01-02 00:00:00"))
  expect_equal(fc$get("fruitmtime"), ts("2026-01-02 00:00:00"))
})
