# Generate a token to enable access to brainmaps API

Generate a token to enable access to brainmaps API

## Usage

``` r
brainmaps_auth(
  client_id = Sys.getenv("BRAINMAPS_CLIENT_ID"),
  client_secret = Sys.getenv("BRAINMAPS_CLIENT_SECRET"),
  scope = "https://www.googleapis.com/auth/brainmaps"
)
```

## Arguments

- client_id, client_secret:

  Client id and secret copied from
  <https://console.developers.google.com>. See details.

- scope:

  The scope for the brainmaps API - you shouldn't need to change this.

## Value

A token that can be used with e.g.
[`GET`](https://httr.r-lib.org/reference/GET.html).

## Details

You will need to activate the brainmaps API, generate a project at
<https://console.developers.google.com> and then request OAuth
credentials, which will create a client id and client secret. The
application name on the console must be `fafbseg`. You should do this
once and then put the keys in your
[`.Renviron`](https://rdrr.io/r/base/Startup.html) file.

- `BRAINMAPS_CLIENT_ID="xxxx"`

- `BRAINMAPS_CLIENT_SECRET="xxxx"`

## See also

See [`.Renviron`](https://rdrr.io/r/base/Startup.html) for how to set
environment variables

## Examples

``` r
if (FALSE) { # \dontrun{
google_token=brainmaps_auth()
# get a list of available volumes
req <- GET("https://brainmaps.googleapis.com/v1beta2/volumes",
  config(token = google_token))
} # }
```
