# GET/POST from brainmaps API with optional retry / cache

`brainmaps_fetch` calls the brainmaps API with optional request cache
and retries.

`brainmaps_clear_cache` clears the cache used by `brainmaps_fetch`

## Usage

``` r
brainmaps_fetch(
  url,
  body = NULL,
  parse.json = TRUE,
  cache = FALSE,
  retry = 0L,
  include_headers = FALSE,
  simplifyVector = TRUE,
  ...
)

brainmaps_clear_cache()
```

## Arguments

- url:

  Full URL for brainmaps API endpoint

- body:

  an R list with parameters that will be converted with
  [`jsonlite::toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  and then passed on to `POST`. You can also pass a `JSON` character
  vector to have more control of the `JSON` encoding.

- parse.json:

  Whether or not to parse a JSON response to an R object (default
  `TRUE`)

- cache:

  Whether or not to cache responses (default `FALSE`)

- retry:

  The number of times to retry the operation (default 0, `FALSE`=\>`0`
  and `TRUE`=\>3). See the documentation of the `times` argument of
  [`httr::RETRY`](https://httr.r-lib.org/reference/RETRY.html) for
  further details.

- include_headers:

  Whether to include basic headers from the http request as attributes
  on the parsed JSON object (default `TRUE`) when `parse.json=TRUE`.

- simplifyVector:

  Whether to use `jsonlite::simplifyVector`

- ...:

  additional arguments passed to the `httr::{RETRY}` function. This may
  include a [`config`](https://httr.r-lib.org/reference/config.html)
  list other named parameters etc.

## Value

An R list parse

## Examples

``` r
if (FALSE) { # \dontrun{
brainmaps_fetch("https://brainmaps.googleapis.com/v1/volumes")
# retry up to 4 times on failure
brainmaps_fetch("https://brainmaps.googleapis.com/v1/volumes", retry=4)
# cache results
brainmaps_fetch("https://brainmaps.googleapis.com/v1/volumes", cache=TRUE)
# use arbitrary curl/httr options (see httr_options())
httr::with_config(httr::verbose(), {
  brainmaps_fetch("https://brainmaps.googleapis.com/v1/volumes")
})
} # }
# \donttest{
brainmaps_clear_cache()
#> [1] TRUE
# }
```
