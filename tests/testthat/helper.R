library(httptest)

skip_on_macos_ci <- function() {
  skip_if(
    nzchar(Sys.getenv("CI")) && identical(Sys.info()[["sysname"]], "Darwin"),
    "Skipping on macOS CI"
  )
}
