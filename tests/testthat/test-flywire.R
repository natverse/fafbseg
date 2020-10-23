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
  expect_true(all(is.na(res[1,1:2])))
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


test_that("can get root ids", {
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if_not_installed('reticulate')
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")

  svids=c("81489548781649724", "80011805220634701")
  expect_named(rootids <- flywire_rootid(svids), svids)
  expect_length(rootids, 2L)
  expect_is(rootids, 'character')
  expect_match(rootids, "^7[0-9]{17}")

  expect_equal(flywire_rootid(svids, method = 'cloudvolume'),
               flywire_rootid(svids, method = 'flywire'))

  expect_equal(flywire_xyz2id(c(102072, 32588, 3778),
                              rawcoords = TRUE,
                              root = FALSE),
               "77618512004398159")
})
