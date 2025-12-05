# GET/POST from flywire (graphene) servers with appropriate authorisation token

GET/POST from flywire (graphene) servers with appropriate authorisation
token

## Usage

``` r
flywire_fetch(
  url,
  body = NULL,
  config = NULL,
  token = NULL,
  return = c("parsed", "text", "response"),
  cache = FALSE,
  retry = 0L,
  include_headers = FALSE,
  simplifyVector = TRUE,
  domain = url,
  ...
)
```

## Arguments

- url:

  Full URL for brainmaps API endpoint

- body:

  an R list with parameters that will be converted with
  [`jsonlite::toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  and then passed on to `POST`. You can also pass a `JSON` character
  vector to have more control of the `JSON` encoding.

- config:

  (optional) curl options, see
  `httr::`[`config`](https://httr.r-lib.org/reference/config.html) for
  details.

- token:

  Optional chunkedgraph token (otherwise the default one for the current
  segmentation will be used). Use `NA` to suppress use of a token.

- return:

  One of "parsed", "text" (for raw JSON), or "response"

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

- domain:

  the domain name for which your CAVE token is valid, defaults to
  \`url\`.

- ...:

  additional arguments passed to the `httr::{RETRY}` function. This may
  include a [`config`](https://httr.r-lib.org/reference/config.html)
  list other named parameters etc.

## Value

Either an R object based on parsing returned JSON, a character vector
containing the raw JSON or a
`httr::`[`response`](https://httr.r-lib.org/reference/response.html)
object, depending on the value of `return`.

## authorisation

Your authorisation will be based on a chunked graph token normally
stored at `~/.cloudvolume/secrets/cave-secret.json`. See
<https://github.com/seung-lab/cloud-volume#cave-secretjson> for the
format. You will need to generate the token as advised by the FlyWire
team. Search or ask for help `#help_software` in the FlyWire slack if
you can't find the information. For more details see article on
[accessing-graphene-server](http://natverse.org/fafbseg/articles/articles/accessing-graphene-server.md).

## Examples

``` r
# \donttest{
# convert a flywire state URL into a parsed neuroglancer scene information
# but see also flywire_expandurl
json=flywire_fetch("https://globalv1.flywire-daf.com/nglstate/5747205470158848",
  return="text")
ngl_segments(ngl_decode_scene(json), as_character = TRUE)
#> [1] "720575940619527173" "720575940628130268" "720575940630484179"
# }
```
