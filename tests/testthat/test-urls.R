context("test-urls")

test_that("decode scene works", {
  expect_error(ngl_decode_scene('https'))
  expect_error(ngl_decode_scene('{'))
})
