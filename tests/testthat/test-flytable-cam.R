test_that("multiplication works", {
  ac=try(flytable_login())
  skip_if(inherits(ac, 'try-error'),
          "skipping flytable tests as unable to login!")

  # sometimes the server seems to give up when requesting a token from each base
  fat <- try(flytable_alltables())
  skip_if(inherits(fat, 'try-error'),
          "skipping flytable tests as having trouble listing all tables!")

  with_segmentation('flywire31', expect_true(nrow(cam_meta('/cell_class:MBON', table = 'info', base='main'))>50 ))
})
