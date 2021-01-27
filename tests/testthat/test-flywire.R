library(fafbseg)
library(httptest)

#set the mocker to re-route..
 set_requester(function (request) {
   gsub_request(request, "https://globalv1.flywire-daf.com/", "api/")
 })


test_that("FlyWire->FAFB works", {
  # identified location in FAFB14
  p.fafb.nm <- cbind(477042, 284535, 90680)
  p.fafb.raw <- p.fafb.nm/c(4,4,40)
  # corresponding location in FlyWire
  p.flywire.raw <- cbind(118865, 71338, 2267)
  p.flywire.nm <- p.flywire.raw * c(4,4,40)

  expect_equal(pt <- flywire2fafb(p.flywire.nm, method = "mapmany"),
               flywire2fafb(p.flywire.nm, method = "map1"))
  # expect sum of displacements to be less than 16 nm
  expect_lt(sum(pt-p.fafb.nm), 16)

  expect_equal(xform_brain(p.flywire.nm, sample="FlyWire", reference = "FAFB14"),
               pt)
})

test_that("FlyWire->FAFB can cope with errors", {
  p.flywire.nm <- matrix(c(477042, 284535, -90680, 477042, 284535, 90680),
                          ncol=3, byrow = T)
  expect_silent(res <- flywire2fafb(p.flywire.nm))
  expect_equal(res[,1], c(NA, 478510))
})

test_that("FAFB->FlyWire works", {
  # identified location in FAFB14
  p.fafb.nm <- cbind(477042, 284535, 90680)
  p.fafb.raw <- p.fafb.nm/c(4,4,40)
  # corresponding location in FlyWire
  p.flywire.raw <- cbind(118865, 71338, 2267)
  p.flywire.nm <- p.flywire.raw * c(4,4,40)

  # expect sum of displacements to be less than 200 nm using swap
  # i.e. worse than forward transform
  expect_warning(pt <- flywire2fafb(p.fafb.nm, swap=TRUE))
  expect_lt(sum(pt-p.flywire.nm), 200)

  expect_silent(pt2 <- fafb2flywire(p.fafb.nm))
  # expect displacement for real transform to be much smaller
  expect_lt(sum(pt2-p.flywire.nm), 16)

  expect_equal(xform_brain(p.fafb.nm, sample="FAFB14", reference = "FlyWire"),
               pt2)
})

#perform recorded mock tests..
with_mock_api(
test_that("check return type/err handles from flywire", {

  mockery::stub(flywire_fetch, 'chunkedgraph_token', 'aabbccdd')
  expect_error(flywire_fetch("https://globalv1.flywire-daf.com/nglstate/123",
                             return="text"),
               class = 'http_502')

  expect_type(flywire_fetch("https://globalv1.flywire-daf.com/nglstate/5747205470158848",
                            return="parsed"),type = 'list')

  expect_type(flywire_fetch("https://globalv1.flywire-daf.com/nglstate/5747205470158848",
                            return="text"), type = 'character')

}))

test_that("can expand a flywire url to get segments", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"), "Skipping live flywire tests")

  expect_equal(
    ngl_segments("https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5409525645443072", as_character = TRUE),
    c("720575940621039145", "720575940626877799"))

})

test_that("flywire url handling", {
  # private function
  expect_match(with_segmentation('sandbox', flywire_cloudvolume_url()),
               "fly_v26")
  expect_match(with_segmentation('flywire', flywire_cloudvolume_url()),
               "fly_v31")
})

test_that("can get root ids", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if_not_installed('reticulate')
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")

  svids=c("81489548781649724", "80011805220634701", "0")
  expect_length(rootids <- flywire_rootid(svids), 3L)
  expect_is(rootids, 'character')
  expect_match(rootids[1:2], "^7[0-9]{17}")

  expect_equal(flywire_rootid(svids, method = 'cloudvolume'),
               flywire_rootid(svids, method = 'flywire'))

  expect_equal(flywire_xyz2id(c(102072, 32588, 3778),
                              rawcoords = TRUE,
                              root = FALSE),
               "77618512004398159")
  expect_warning(flywire_xyz2id(c(102072, 32588, 3778)), '.*raw')
  expect_equal(
    id <- flywire_xyz2id(c(158961, 70514, 2613), rawcoords = T, root=TRUE),
    expect_warning(flywire_xyz2id(c(158961, 70514, 2613), rawcoords = T, root=TRUE, fast_root = FALSE))
    )

  # current as of 10 Nov 2020
  expect_equal(id, "720575940621039145")
  # check flywire_latestid vs mapping an xyz location
  with_segmentation('sandbox',
                    expect_equal(
                      # defined by an XYZ location
                      flywire_xyz2id(c(158961, 70514, 2613), rawcoords = T),
                      # defined by an old root id which has been superseded
                      flywire_latestid('720575940610453042')
                    ))

})

test_that("can get flywire change logs", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  kcs=bit64::as.integer64(c("720575940609992371","720575940623755722"))
  expect_is(df <- flywire_change_log(kcs), 'data.frame')
  expect_named(df, c("id", "operation_id", "timestamp", "user_id", "is_merge", "user_name"))
  expect_equal(nrow(df), 4)
})


test_that("can get flywire supervoxels", {
  expect_length(ll <- flywire_leaves('720575940623755722'), 8536)
  skip_if_not_installed('bit64')
  ll64 <- bit64::as.integer64(ll)
  expect_equal(flywire_leaves('720575940623755722', integer64 = TRUE), ll64)
  expect_known_hash(ll64, hash = "2b39aafdc8")

  ids2=c('720575940623755722','720575940616243077')
  expect_is(l2 <- flywire_leaves(ids2, integer64 = T), 'list')
  expect_named(l2, ids2)
  expect_equal(l2[[1]], ll64)
})

test_that("can check if flywire root ids are current", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")

  ids=c("720575940619073968", "720575940637707136")
  expect_equal(flywire_islatest(ids),
               rep(FALSE, 2))
  expect_equal(flywire_islatest(bit64::as.integer64(ids)),
               rep(FALSE, 2))
})



test_that("can parse save states", {
  skip("TODO: implement parsing of save states")
  expect_is(df <-
              flywire_save_states(testthat::test_path("testdata/FlyWire.html")),
            'data.frame')
  expect_true(nrow(df)==4L)
  expect_named(df, c("timestamp", "url"))
})

test_that("valid_id works",{
  expect_true(all(valid_id(integer())))
  expect_true(all(valid_id(bit64::integer64())))
  expect_true(all(valid_id(bit64::as.integer64(1L))))
  expect_true(all(valid_id(0:1L)))
  expect_false(all(valid_id(-1:4)))
  expect_false(valid_id(bit64::NA_integer64_))
  expect_false(valid_id(NA_real_))
  expect_false(valid_id(NA))
  expect_true(all(valid_id(c(0, 1, 2.0^52))))
  expect_false(all(valid_id(2.0^60)))
  expect_true(valid_id("9223372036854775807"))
  expect_false(valid_id("99223372036854775807"))
})
