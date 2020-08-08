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
