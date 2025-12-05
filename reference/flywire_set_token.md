# Record (and if necessary create) a FlyWire chunkedgraph token

`chunkedgraph_token` returns the current chunked graph token, optionally
for a specified URL / domain name.

Writes a token to a standard location on disk so that it will be found
by the cloudvolume python package as well as the fafbseg package and
used to authenticate to <https://flywire.ai>.

## Usage

``` r
chunkedgraph_token(url = NULL, cached = TRUE)

flywire_set_token(token = NULL, domain = NULL)
```

## Arguments

- url:

  A URL or domain name which defines the scope of the token

- cached:

  Whether to use a cached version of the token (default yes for speed,
  but set to FALSE to reload after writing a new token to disk).

- token:

  Optional character vector containing your token. If missing, a new
  token will be requested (note that this will invalidate your previous
  token)

- domain:

  Optional fully qualified domain name, which will be prepended to the
  filename in which the token will be stored.

## Value

character vector containing the token (typically 32-44 bytes)

The path to the file storing the token (invisibly)

## Details

Since cloudvolume 3.10.0 April 2021, the recommended token filenames
look like

- `'cave-secret.json'`

- `'cave-secret.json'`

## Examples

``` r
if (FALSE) { # \dontrun{
# Will open browser to get new token
flywire_set_token()
# Writes a known token to correct location
flywire_set_token("2f88e16c4f21bfcb290b2a8288c05bd0")
} # }
```
