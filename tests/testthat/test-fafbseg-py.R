test_that("multiplication works", {
  skip_if_not_installed('reticulate')
  token=try(chunkedgraph_token(), silent = TRUE)
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire L2 tests")
  skip_if_not(reticulate::py_module_available("fafbseg"),
              "Skipping live flywire L2 tests requiring python fafbseg module")

  kcsvids=c("78603674556915608", "78462662124123765", "77547662357982001",
  "78533168373869635", "78251418452635714", "78323024281482155",
  "78322062208411707", "78533649477402370", "77829412279715493",
  "77899643517979532", "78814230967028270", "78533993141739277",
  "78041274292494941", "78252449311896359", "77618924522629940",
  "77618237260576979", "78673768356594679", "78182148951479619",
  "78392293379997680", "77688812230426430")
  kcids=flywire_rootid(kcsvids)
  expect_is(kcs <- read_l2skel(kcids[1:5]), 'neuronlist')
  expect_is(kcs[[1]], 'neuron')
  expect_is(kcdps <- read_l2dp(kcids[1:5]), 'neuronlist')
  expect_is(kcdps[[1]], 'dotprops')
})
