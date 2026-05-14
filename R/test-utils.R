skip_if_flywire_materialize_unavailable <- function(
    message = "Skipping live FlyWire tests: materialize service unavailable") {
  cave_versions <- try({
    capture.output(
      capture.output(
        invisible(flywire_cave_client()$materialize$get_versions(expired = TRUE)),
        type = "message"
      ),
      type = "output"
    )
    TRUE
  }, silent = TRUE)
  testthat::skip_if(!isTRUE(cave_versions), message)
}
