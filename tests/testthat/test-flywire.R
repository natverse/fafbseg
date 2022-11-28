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
  p.fafb.raw <- flywire_nm2raw(p.fafb.nm)
  # corresponding location in FlyWire
  p.flywire.raw <- cbind(118865, 71338, 2267)
  p.flywire.nm <- flywire_raw2nm(p.flywire.raw)
  expect_equal(flywire_raw2nm(p.flywire.raw, vd=c(8,8,80)),
               p.flywire.nm*2)

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

  u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5409525645443072"
  expect_equal(
    ngl_segments(u, as_character = TRUE),
    c("720575940621039145", "720575940626877799"))
  expect_equal(ngl_segments(flywire_shortenurl(u)),
               c("720575940621039145", "720575940626877799"))

  # from Forrest
  u2="https://neuromancer-seung-import.appspot.com/?json_url=https://globalv1.daf-apis.com/nglstate/api/v1/4784519232094208"
  expect_is(sc2 <- ngl_decode_scene(u2), 'ngscene')
  expect_equal(
    ngl_segments(u2),
    c(
      "720575940496762311",
      "720575940604467744",
      "720575940614325947",
      "720575940617180982",
      "720575940620632926",
      "720575940623345907",
      "720575940629402319",
      "720575940631521363",
      "720575940637384518"
    )
  )

  expect_error(
    flywire_expandurl(
      fafbseg::choose_segmentation('flywire', set = F)$fafbseg.sampleurl
    ),
    'shortened neuroglancer'
  )
  expect_known_hash(flywire_expandurl('https://tinyurl.com/rmr58jpn'),
                    hash = 'a5fb89f6f9')
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

  expect_equal(flywire_rootid(svids),
               flywire_rootid(bit64::as.integer64(svids)))

  expect_equal(flywire_rootid(svids, method = 'cave'),
               flywire_rootid(svids, method = 'flywire'))

  expect_equal(
    flywire_rootid(svids, method = 'cave', integer64 = T, version=526),
    flywire_rootid(svids, method = 'cloudvolume', integer64 = T, version=526))

  expect_equal(
    flywire_rootid(svids, method = 'cave', stop_layer = 2, version=526),
    flywire_rootid(svids, method = 'cloudvolume', stop_layer = 2, version=526))

  expect_equal(flywire_xyz2id(c(102072, 32588, 3778),
                              rawcoords = TRUE,
                              root = FALSE),
               "77618512004398159")
  expect_warning(flywire_xyz2id(c(102072, 32588, 3778)), '.*raw')
  ac <- c(
    "(123937.0075,37750.52,4424)",
    "(116598.155,49311.5825,5184)",
    "(120042.15,55131.8325,5369)"
  )
  expect_type(flywire_xyz2id(ac, rawcoords = TRUE), 'character')
  expect_error(flywire_xyz2id(as.character(1:4)))
  expect_error(flywire_xyz2id(as.character(1:3)))

  expect_equal(
    id <- flywire_xyz2id(c(158946, 43428, 3523), rawcoords = T, root=TRUE),
    expect_warning(flywire_xyz2id(c(158946, 43428, 3523),
                                  rawcoords = T, root=TRUE, fast_root = FALSE))
  )

  # current as of 28 April 2021
  expect_equal(id, "720575940626657808")

  expect_equal(flywire_latestid('720575940622465800', method='cave'),
               lid <- flywire_latestid('720575940622465800', method='leaves'))

  expect_equal(flywire_latestid(c('720575940622465800', NA), method='leaves'),
               c(lid, 0))

  kcs=data.frame(
    rootid=c("720575940615471505", "720575940602564320", "720575940602605536"),
    xyz=c("(159284,42762,3594)", "(159035,41959,3594)", "(157715,44345,3594)")
  )
  # update root ids
  expect_message(flywire_updateids(kcs$rootid, xyz=kcs$xyz, rawcoords = T), "Updating")
  kcs[4,]=c("0", "(NA,NA,NA)")
  kcs$svid=flywire_xyz2id(kcs$xyz, rawcoords = T)
  expect_equal(
    expect_warning(
      flywire_updateids(kcs$rootid, xyz=kcs$xyz, rawcoords = T, Verbose = F)
    ),
    flywire_updateids(kcs$rootid, svids=kcs$svid, Verbose = F)
  )

  expect_warning(flywire_updateids(kcs$rootid, xyz=kcs$xyz, svids=kcs$svid,
                                   rawcoords = T, Verbose = F),
                 "using svids")

  # dl1ids=flywire_ids("DL1_adPN") %>% sort()
  dl1ids=c("720575940622368792", "720575940627042064", "720575940629656535",
           "720575940632167085")
  dl1ids.401=c("720575940618302936", "720575940627042064", "720575940618757681",
               "720575940632167085")
  dl1.svids=c("77337105814452184", "80857191821269694", "78534748452308679",
              "80927491845825492")
  expect_warning(expect_equal(flywire_updateids(dl1ids, version = 401), dl1ids.401),
                 regexp = "Falling back")

  expect_equal(flywire_updateids(dl1ids, svids = dl1.svids, version = 401),
               dl1ids.401)

  expect_warning(flywire_updateids(NA, svids=NA), "unable to update 1")

  # check flywire_latestid vs mapping an xyz location
  with_segmentation('sandbox',
                    expect_equal(
                      # defined by an XYZ location
                      flywire_xyz2id(c(158946, 43428, 3523), rawcoords = T),
                      # defined by an old root id which has been superseded
                      flywire_latestid('720575940625602908')
                    ))

})

test_that("can get flywire change logs", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  kcs=bit64::as.integer64(c("720575940609992371","720575940623755722"))
  expect_is(df <- flywire_change_log(kcs), 'data.frame')
  expect_true(all(c("id", "operation_id", "timestamp", "user_id", "is_merge", "user_name") %in% names(df)))
  expect_equal(nrow(df), 4)
})


test_that("can get flywire supervoxels", {
  expect_length(ll <- flywire_leaves('720575940623755722'), 8536)
  expect_length(flywire_leaves('0'),0L)
  skip_if_not_installed('bit64')
  ll64 <- bit64::as.integer64(ll)
  expect_equal(flywire_leaves('720575940623755722', integer64 = TRUE), ll64)

  s10=bit64::as.integer64(c("80925361676164750", "81348742708098299", "81419042799875117",
    "81278442750510441", "80996142737348972", "81278442817014605",
    "81137017664569012", "79871273555441841", "81278511536415558",
    "80082379854889799"))

  expect_true(all(s10%in%ll64))

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

  kcs=bit64::as.integer64(c("720575940625507888", "720575940626474889"))
  expect_is(uptodate <- flywire_islatest(kcs), 'logical')
  expect_equal(flywire_islatest(c(kcs, NA)), c(uptodate, NA))

  if(!any(uptodate))
    skip("Skipping flywire_latestid check because no input ids are current")
  expect_equal(kcs[uptodate], flywire_latestid(kcs[uptodate]))
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
  expect_true(valid_id(NA_character_, na.ok = TRUE))
  expect_true(valid_id(NA_integer_, na.ok = TRUE))
  expect_true(valid_id(bit64::as.integer64("NA"), na.ok = TRUE))
})


test_that('flywire_last_modified works', {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  expect_equal(flywire_last_modified("720575940619073968"),
               as.POSIXct(1606909381.505, origin='1970-01-01', tz = "UTC"))
})
