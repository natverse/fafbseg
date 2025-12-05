# Read neuroglancer data dump from disk / web

Read neuroglancer data dump from disk / web

## Usage

``` r
read_ng_dump(x, ...)
```

## Arguments

- x:

  Path to directory, zip file, or set of data files

- ...:

  Additional argument passed to
  [`read_ng_raw`](https://natverse.org/fafbseg/reference/read_ng_raw.md)

## Value

A list of class `ng_raw_list` with additional metadata as attributes.

## Details

A neuroglancer data dump consists of a mix of JSON and custom binary
data files, which together define mesh data for (fragments) of neurons
together with associated metadata. See
[`fetch_all_curl`](https://natverse.org/fafbseg/reference/fetch_all_curl.md)
for how to prepare such a data dump.

## See also

[`fetch_all_curl`](https://natverse.org/fafbseg/reference/fetch_all_curl.md),
[`read_ng_raw`](https://natverse.org/fafbseg/reference/read_ng_raw.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_all_curl("all_curl.sh", outdir="alldata",
  regex="brainmaps.googleapis.com", fixed=TRUE)
meshdata=read_ng_dump("alldata")
m=as.mesh3d(meshdata)
shade3d(m, col='red')

# can also be a (remote) zip file
meshdata=read_ng_dump("https://myfiles.com/myneuron.zip")
} # }
```
