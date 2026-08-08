test_that("query works", {

  ac=try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable tests as unable to login!")

  # sometimes the server seems to give up when requesting a token from each base
  fat <- try(flytable_alltables())
  skip_if(inherits(fat, 'try-error'),
          "skipping flytable tests as having trouble listing all tables!")
  skip_if_flywire_materialize_unavailable(
    "skipping flytable tests as FlyWire materialize service is unavailable"
  )

  # queries fly table for cell types
  expect_equal(dl4ids <- flywire_ids('DL4_adPN_L', version=630), "720575940627708688")
  expect_true(length(flywire_ids('class:MBON', integer64 = T))>90)

  expect_equal(mbon0x <- flytable_cell_types('MBON0%'),
               flytable_cell_types('/type:MBON0[1-9]', table = 'info'))
  expect_equal(flytable_meta(mbon0x), mbon0x)
  expect_true(length(flywire_ids('super:sensory', integer64 = T))>1000)
  expect_error(flywire_ids('pudding:sensory'))

  tf=tempfile('info.json')
  expect_silent(fct2nginfo(f=tf, ids = 'MBON%', gluestr = "{cell_type}_{toupper(substr(side,1,1))}"))
  expect_true(is.list(l <- read_nginfo(tf)))
  expect_equal(l$inline$ids, flywire_ids('MBON%'))

  expect_s3_class(df <- flytable_query("select fruit_name, person, _ctime, date_wminute FROM testfruit WHERE nid<=3", limit=3L),
                  'data.frame')
  expect_equal(nrow(df), 3L)
  expect_s3_class(fruit <- flytable_list_rows('testfruit'), 'data.frame')
  expect_equal(flytable_list_rows('testfruit', limit=3), fruit[1:3,])
  expect_equal(flytable_list_rows('testfruit', limit=3, chunksize = 2), fruit[1:3,])

  # This test fails sporadically when multiple processes are accessing the db
  # so I think we have to skip
  # expect_equal(flytable_nrow('testfruit'), nrow(fruit))
  # same representation via flytable_list_rows or flytable_query
  expect_equal(fruit[rownames(df),colnames(df)], df)

  expect_true(
    flytable_update_rows(table = 'testfruit',
                         fruit[min(4, nrow(fruit)),
                                   c("_id", "fruit_name", "person", "nid")],
                         chunksize = 1))
  # use a random id to avoid race conditions with other processes
  nid=sample.int(1e7, size = 1, replace = T)
  expect_true(flytable_append_rows(
    table = 'testfruit',
    data.frame(fruit_name='kiwi', person='Frederick the Great', nid=nid)))

  # now delete that row
  qu=glue::glue("SELECT '_id' FROM testfruit WHERE person='Frederick the Great' AND nid={nid}")
  expect_true(nrow(iddf <- flytable_query(qu))>0)

  if(nrow(iddf)>10) {
    Sys.sleep(3)
    flytable_delete_rows(iddf[['_id']], table = 'testfruit', DryRun = F)
  }
  # make a fake neuronlist
  nl=Cell07PNs[seq_along(dl4ids)]
  nl[,]=NULL
  names(nl)=dl4ids
  expect_warning(add_celltype_info(nl, version = 630, suffix = '.y', table = 'info'))

  # check we can get ids from info table
  expect_equal(flywire_ids('LT33', version = 571),
               c("720575940615952450", "720575940634931552"))

  # check handling of unique elements. Lots of duplicates for glia
  expect_warning(
    glialinfou <-
      flytable_meta(
        ids = 'cell_class:putative_glia',
        table = 'info',
        unique = T
      )
  )
  expect_true(all(
    flytable_meta(
      ids = 'cell_class:putative_glia',
      table = 'info',
      unique = F
    )$root_id %in% glialinfou$root_id
  ))

  expect_equal(lt33 <- flywire_ids('LT33', version = 630),
               c("720575940646126190", "720575940627348057"))

  expect_equal(
    withr::with_options(list(fafbseg.use_static_celltypes=T),
                        flywire_ids('LT33', version = 630)),
    lt33)

  expect_equal(
    withr::with_options(list(fafbseg.use_static_celltypes=T),
                        flytable_meta('720575940625808642', version = 630)$side),
    'left')
})


test_that("flytable_query paginates against a real server", {
  ac=try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable pagination tests as unable to login!")

  # `full` fetches testfruit in the normal (single-call) way; `paged` forces
  # the LIMIT/OFFSET paging path in small windows. Same server, same table, so
  # the stitched-together paged result must reproduce the full one. (The
  # Cambridge server's own row cap is far larger than testfruit, so chunksize
  # is the only way to exercise real multi-page stitching without a huge
  # table.)
  full <- try(flytable_query("select * from testfruit"))
  skip_if(inherits(full, 'try-error') || is.null(full) || nrow(full) < 2,
          "skipping: testfruit not available")

  paged <- flytable_query("select * from testfruit", chunksize = 20)
  expect_equal(nrow(paged), nrow(full))
  expect_setequal(paged[["_id"]], full[["_id"]])
  # content check: representative columns match row-for-row in _id order.
  # Compared as character to sidestep column-type inference legitimately
  # differing between a single fetch and a stitched multi-page one (an all-NA
  # column in one 20-row page comes back logical, character in the full set).
  cols <- intersect(c("fruit_name", "person", "nid"), colnames(full))
  as_key <- function(df) {
    df <- df[order(df[["_id"]]), cols, drop = FALSE]
    data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  }
  expect_equal(as_key(paged), as_key(full))

  # flytable_full_fetch drives its own count-based paging loop; force it to
  # take multiple pages over the same small table.
  ff <- fafbseg:::flytable_full_fetch("testfruit", chunksize = 20)
  expect_equal(nrow(ff), nrow(full))
  expect_setequal(ff[["_id"]], full[["_id"]])
  expect_false(is.null(attr(ff, "mtime")))

  # Exercise the production auto-detect path (no chunksize) against a table
  # larger than guaranteed_cap: a first full page followed by a probe that
  # confirms there is nothing beyond it. `info` has tens of thousands of rows,
  # so this would come back truncated at 10k were the cap logic broken.
  info_ids <- try(flytable_query("select _id from info"))
  if (!inherits(info_ids, 'try-error') && !is.null(info_ids))
    expect_gt(nrow(info_ids), 10000)
})


test_that("read only shared tables", {
  # check we can handle situation where user is not a full member of workspace
  # but just has access to a specific shared table
  # user
  ac=fafbseg::flytable_login(token = '22791a98a299312d32539254430ab436bd59a3e7')
  expect_true("info"%in%flytable_alltables(ac)$name)
})

test_that("workspace list duplicates are removed before table fetch", {
  memoise::forget(flytable_workspaces_impl)

  ac <- list(
    list_workspaces = function() list(
      workspace_list = list(
        list(
          table_list = data.frame(
            workspace_id = 148,
            name = "aedes",
            stringsAsFactors = FALSE
          ),
          shared_table_list = data.frame(
            workspace_id = 148,
            name = "aedes",
            stringsAsFactors = FALSE
          )
        ),
        list(
          table_list = data.frame(
            workspace_id = 5,
            name = "main",
            stringsAsFactors = FALSE
          ),
          shared_table_list = data.frame(
            workspace_id = integer(),
            name = character(),
            stringsAsFactors = FALSE
          )
        )
      )
    )
  )

  wsdf <- flytable_workspaces_impl(ac)

  expect_equal(sum(wsdf$workspace_id == 148 & wsdf$name == "aedes"), 1L)
  expect_equal(sum(wsdf$workspace_id == 5 & wsdf$name == "main"), 1L)
})


test_that("delta sync timestamp handling is correct", {
  # Truncating fractional seconds for datedif query
  mtime_nano <- "2026-03-27T14:36:41.382928045Z"
  sync_from <- sub("\\.\\d+", "", mtime_nano)
  expect_equal(sync_from, "2026-03-27T14:36:41Z")

  # No fractional seconds — unchanged
  mtime_whole <- "2026-03-27T14:36:41Z"
  expect_equal(sub("\\.\\d+", "", mtime_whole), mtime_whole)

  # has_modifications comparison: truncated cached_time should not miss
  # same-second modifications (max_mtime lacks sub-second precision)
  cached_time <- fafbseg:::flytable_parse_date(mtime_nano, format = 'timestamp')
  max_mtime <- fafbseg:::flytable_parse_date("2026-03-27T14:36:41Z", format = 'timestamp')

  # Without truncation, this would be FALSE (sub-second precision makes cached > max)
  expect_false(max_mtime > cached_time)
  # With truncation, same-second is caught
  expect_true(max_mtime >= trunc(cached_time, units = 'secs'))
})


test_that("multi-select comma-string shorthand splits correctly", {
  listify <- fafbseg:::flytable_listify_multiselect_col

  # plain scalar, no comma
  expect_equal(listify("AB", "initials"), list("AB"))

  # comma-joined shorthand -> split into multiple values
  expect_equal(listify("AB,CD", "initials"), list(c("AB", "CD")))
  # whitespace around a comma is trimmed
  expect_equal(listify("AB, CD", "initials"), list(c("AB", "CD")))
  # repeated/trailing commas don't produce empty tokens
  expect_equal(listify("AB,,CD", "initials"), list(c("AB", "CD")))
  expect_equal(listify("AB,", "initials"), list("AB"))

  # NA / empty -> cleared cell
  expect_equal(listify(c(NA_character_, ""), "initials"),
              list(character(0), character(0)))

  # vectorised across rows
  expect_equal(listify(c("AB", "AB,CD", NA), "initials"),
              list("AB", c("AB", "CD"), character(0)))

  # a list-column cell is taken verbatim, NOT split -- this is how a
  # literal option name that itself contains a comma is written
  expect_equal(listify(list("AB,CD"), "initials"), list("AB,CD"))
  expect_equal(listify(list(c("AB,CD", "EF")), "initials"),
              list(c("AB,CD", "EF")))
})


test_that("delta sync row update handles POSIXct columns", {
  # R's [<-.data.frame with whole-row assignment fails with mixed POSIXct/other
  # columns. Explicitly specifying columns avoids the bug.
  cached <- data.frame(
    id = c("a", "b", "c"),
    value = 1:3,
    mtime = as.POSIXct(c("2026-01-01", "2026-01-02", "2026-01-03"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  fresh <- data.frame(
    id = "b",
    value = 99L,
    mtime = as.POSIXct("2026-03-27", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  cols <- colnames(fresh)
  cached[2, cols] <- fresh[1, cols, drop = FALSE]
  expect_equal(cached$value[2], 99L)
  expect_equal(cached$mtime[2], as.POSIXct("2026-03-27", tz = "UTC"))
})


test_that("flytable_cached_table works", {
  ac <- try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable_cached_table tests as unable to login!")

  fat <- try(flytable_alltables())
  skip_if(inherits(fat, 'try-error'),
          "skipping flytable_cached_table tests as having trouble listing all tables!")

  # Clear any existing cache for testfruit
  fc <- fafbseg:::flytable_cache()
  cache_key <- fafbseg:::flytable_cache_key('testfruit')
  fc$remove(cache_key)

  # Test 1: Basic fetch (cache miss)
  fruit1 <- flytable_cached_table('testfruit')
  expect_s3_class(fruit1, 'data.frame')
  expect_true(nrow(fruit1) > 0)
  expect_true(!is.null(attr(fruit1, 'mtime')))

  # Test 2: Cache hit within expiry window
  fruit2 <- flytable_cached_table('testfruit', expiry = 3600)
  expect_equal(fruit1, fruit2)
  expect_equal(attr(fruit1, 'mtime'), attr(fruit2, 'mtime'))

  # Test 3: Force sync with expiry = 0
  fruit3 <- flytable_cached_table('testfruit', expiry = 0)
  expect_s3_class(fruit3, 'data.frame')
  expect_equal(nrow(fruit3), nrow(fruit1))

  # Test 4: Force complete refresh
  fruit4 <- flytable_cached_table('testfruit', refresh = TRUE)
  expect_s3_class(fruit4, 'data.frame')
  expect_true(!is.null(attr(fruit4, 'mtime')))

  # Test 5: mtime attribute is a valid timestamp
  mtime <- attr(fruit4, 'mtime')
  expect_true(is.character(mtime))
  expect_true(nchar(mtime) > 10)  # Should be a proper timestamp string

  # Cleanup
  fc$remove(cache_key)
})


test_that("flytable_cached_table delta sync picks up new rows", {
  ac <- try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping delta sync test as unable to login!")

  fat <- try(flytable_alltables())
  skip_if(inherits(fat, 'try-error'),
          "skipping delta sync test as having trouble listing all tables!")

  fc <- fafbseg:::flytable_cache()
  cache_key <- fafbseg:::flytable_cache_key('testfruit')
  fc$remove(cache_key)

  # Baseline: full fetch
  fruit_before <- flytable_cached_table('testfruit')
  n_before <- nrow(fruit_before)

  # Append a row with a unique nid to avoid collisions
  nid <- sample.int(1e7, size = 1)
  res <- try(flytable_append_rows(
    table = 'testfruit',
    data.frame(fruit_name = 'dragonfruit', person = 'Delta Sync Test', nid = nid)),
    silent = TRUE)
  skip_if(inherits(res, 'try-error'), "skipping: row append failed")

  # Delta sync should pick up the new row; use >= because concurrent edits
  # to testfruit can add or remove other rows between baseline and sync
  fruit_after <- flytable_cached_table('testfruit', expiry = 0)
  expect_gte(nrow(fruit_after), n_before + 1L)
  expect_true(any(fruit_after$nid == nid))

  # Verify mtime was updated (sync was complete)
  expect_true(attr(fruit_after, 'mtime') != attr(fruit_before, 'mtime'))

  # Cleanup: delete the test row
  iddf <- flytable_query(
    glue::glue("SELECT `_id` FROM testfruit WHERE person='Delta Sync Test' AND nid={nid}"))
  if (nrow(iddf) > 0) {
    flytable_delete_rows(iddf[['_id']], table = 'testfruit', DryRun = FALSE)
  }
  fc$remove(cache_key)
})


test_that("multi-select column writes work", {
  ac <- try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping multi-select tests as unable to login!")

  # use values that already exist in the vocabulary for the happy-path
  # checks, so those don't depend on allow_new_options
  opts <- flytable_select_options('testfruit', 'initials')$initials
  skip_if(length(opts) < 2,
          "skipping: need >= 2 existing options on testfruit.initials")
  ab <- opts[1]; cd <- opts[2]

  # work on a dedicated row so we never touch anyone else's data. Set
  # initials on append (rather than leaving it unset) so this also
  # exercises df2appendpayload()'s multi-select JSON-array branch, which is
  # otherwise never touched by the update-focused checks below
  nid <- sample.int(1e7, size = 1)
  res <- try(flytable_append_rows(
    table = 'testfruit',
    data.frame(fruit_name = 'starfruit', person = 'Multi Select Test', nid = nid,
              initials = I(list(ab)))),
    silent = TRUE)
  skip_if(inherits(res, 'try-error'), "skipping: row append failed")

  iddf <- flytable_query(glue::glue(
    "SELECT `_id`, initials FROM testfruit WHERE person='Multi Select Test' AND nid={nid}"))
  skip_if(nrow(iddf) == 0, "skipping: could not find freshly appended row")
  row_id <- iddf[['_id']][1]
  on.exit(flytable_delete_rows(row_id, table = 'testfruit', DryRun = FALSE), add = TRUE)
  expect_equal(iddf$initials[1], ab)

  # 1. a single scalar value round-trips
  expect_true(flytable_update_rows(
    table = 'testfruit',
    data.frame(row_id = row_id, initials = ab, stringsAsFactors = FALSE)))
  expect_equal(flytable_query(glue::glue(
    "SELECT initials FROM testfruit WHERE `_id`='{row_id}'"))$initials, ab)

  # 2. a genuine multi-value write round-trips (order-independent)
  expect_true(flytable_update_rows(
    table = 'testfruit',
    data.frame(row_id = row_id, initials = I(list(c(ab, cd))))))
  written <- flytable_query(glue::glue(
    "SELECT initials FROM testfruit WHERE `_id`='{row_id}'"))$initials
  expect_setequal(strsplit(written, ",")[[1]], c(ab, cd))

  # 3. clearing a populated cell reads back as NA (the row already had a
  # multi-value cell from step 2, so this is testing a real clear, not a
  # no-op on an already-empty cell)
  expect_true(flytable_update_rows(
    table = 'testfruit',
    data.frame(row_id = row_id, initials = I(list(character(0))))))
  expect_true(is.na(flytable_query(glue::glue(
    "SELECT initials FROM testfruit WHERE `_id`='{row_id}'"))$initials))

  # 4. a comma-joined string is accepted as shorthand and split into
  # multiple values -- symmetric with how a multi-select cell reads back by
  # default. Each split token still goes through the same vocabulary guard
  # as any other value, so this doesn't reopen the door to bogus options.
  expect_true(flytable_update_rows(
    table = 'testfruit',
    data.frame(row_id = row_id, initials = paste(ab, cd, sep = ","),
              stringsAsFactors = FALSE)))
  written2 <- flytable_query(glue::glue(
    "SELECT initials FROM testfruit WHERE `_id`='{row_id}'"))$initials
  expect_setequal(strsplit(written2, ",")[[1]], c(ab, cd))

  # 5. vocab guard rejects an unknown option name by default, and the
  # error names the offending value plus both remediation paths. No
  # verifying read needed -- the write errors before it reaches seatable.
  err <- expect_error(flytable_update_rows(
    table = 'testfruit',
    data.frame(row_id = row_id, initials = I(list("totally-new-option")))))
  expect_match(conditionMessage(err), "totally-new-option", fixed = TRUE)
  expect_match(conditionMessage(err), "flytable_add_select_options", fixed = TRUE)
  expect_match(conditionMessage(err), "allow_new_options", fixed = TRUE)

  # 6. allow_new_options = TRUE actually adds the option via the seatable
  # API and the write then succeeds. Use a fresh, clearly-tagged option
  # name every run so this genuinely exercises flytable_add_select_options()
  # each time rather than skipping it once the option already exists --
  # the seatable_api python package has no documented option-removal call,
  # so these test options are left in place permanently on testfruit, the
  # same way the UI-added probe options documented in
  # multiselect-write-plan.md had to be cleaned up manually.
  new_opt <- paste0("zztest-allow-new-options-", format(Sys.time(), "%Y%m%d%H%M%OS3"))
  expect_true(flytable_update_rows(
    table = 'testfruit',
    data.frame(row_id = row_id, initials = I(list(new_opt))),
    allow_new_options = TRUE))
  expect_true(new_opt %in% flytable_select_options('testfruit', 'initials')$initials)
})
