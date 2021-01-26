context("test-merge-groups")

test_that("find agglomeration groups for google segmentation", {
  skip_if_not_installed('fafbsegdata')
  skip_on_os("mac")

  # confirmed by Peter Li Sep 2018
  # see https://fafb.slack.com/archives/GCSHW2YFK/p1537818312000100?thread_ts=1537809139.000100&cid=GCSHW2YFK
  peterli_segs <-
    as.character(c(
      7186840767,
      7186844852,
      7188488573,
      7188488876,
      7506547847,
      7511521487,
      7513174533,
      7523125878,
      7524769973,
      7528082928,
      7528082999,
      7528083090,
      7528087811,
      7844472994,
      7844473351,
      7866011642,
      7866015978,
      7869320680,
      8184049304,
      8184049546,
      8187380528,
      8193975632,
      8198954037,
      8515392737,
      8525318026,
      8525318518,
      8525318619,
      8525318881,
      8526948746,
      8535280040,
      8541870917,
      8543523319,
      8861569429,
      8866569630,
      8866569764,
      8869882415,
      8873165227,
      8873192134,
      8874821377,
      8876521709,
      8876543998,
      8878156810,
      8881456848,
      8883135153,
      9217782462,
      9219408058,
      9219408083,
      9221047258,
      9537450356,
      9539102248,
      9540758370,
      9540758400,
      9540758403,
      9554037005,
      9554041768,
      9555689874,
      9560632655,
      9562293635,
      9563945725,
      9875383416,
      9880358224,
      9885314546,
      10218295532,
      10233257420
    ))

  expect_equal(find_merged_segments(peterli_segs[1]), sort(peterli_segs))
  expect_equal(sort(find_merged_segments(peterli_segs[2])), sort(peterli_segs))
  expect_equal(sort(find_merged_segments(peterli_segs[2:1])), sort(peterli_segs))

  expect_equal(find_merged_segments(peterli_segs[1], return.groups = TRUE,
                                    return.segmentids.for.groups = FALSE)[['group']],
               rep(391213L, length(peterli_segs)))
})
