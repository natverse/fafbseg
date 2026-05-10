test_that("flywire nuclei works", {
  nuclei_trace <- function(...) {
    if(exists("mac_ci_trace", mode = "function"))
      mac_ci_trace(...)
    invisible(NULL)
  }

  nuclei_trace("nuclei: before chunkedgraph_token")
  token=try(chunkedgraph_token(), silent = TRUE)
  nuclei_trace("nuclei: after chunkedgraph_token; try-error=", inherits(token, "try-error"))
  skip_if_not_installed('reticulate')
  skip_if(inherits(token, "try-error"),
          "Skipping live flywire tests")
  nuclei_trace("nuclei: before py_module_available(cloudvolume)")
  skip_if_not(reticulate::py_module_available("cloudvolume"),
              "Skipping live flywire tests requiring python cloudvolume module")
  nuclei_trace("nuclei: after py_module_available(cloudvolume)")

  # make sure this an up to date root id
  nuclei_trace("nuclei: before flywire_rootid")
  rid=flywire_rootid('80999991094644060')
  nuclei_trace("nuclei: after flywire_rootid; rid=", rid)
  nuclei_trace("nuclei: before flywire_nuclei")
  n1 <- flywire_nuclei(rid, rawcoords = T)
  nuclei_trace("nuclei: after flywire_nuclei; rows=", nrow(n1),
               "; cols=", paste(names(n1), collapse = ","))
  expect_equal(nrow(n1), 1L)
  nuclei_trace("nuclei: before flywire_nearest_nuclei")
  nearest <- flywire_nearest_nuclei(n1$pt_position, rawcoords = T)
  nuclei_trace("nuclei: after flywire_nearest_nuclei; rows=", nrow(nearest),
               "; cols=", paste(names(nearest), collapse = ","))
  expect_equal(nearest[1:ncol(n1)], n1)
})
