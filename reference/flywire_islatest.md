# Check that one or more FlyWire root ids have not been further edited

You can think of `flywire_islatest` as returning whether a root id is
valid at a given timestamp (by default now). If the corresponding object
has been edited, invalidating the root id, or does not (yet) exist then

## Usage

``` r
flywire_islatest(
  x,
  cloudvolume.url = NULL,
  timestamp = NULL,
  version = NULL,
  cache = NA,
  ...
)
```

## Arguments

- x:

  FlyWire rootids in any format understandable to
  [`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  including as `integer64`

- cloudvolume.url:

  URL for CloudVolume to fetch segmentation image data. The default
  value of NULL chooses the flywire production segmentation dataset.

- timestamp:

  (optional) argument to set an endpoint - edits after this time will be
  ignored (see details).

- version:

  An optional CAVE materialisation version number. See details and
  examples.

- cache:

  Whether to cache the result - the default value of `NA` will do this
  if a `timestamp` or `version` argument is supplied.

- ...:

  Additional arguments to
  [`flywire_fetch`](https://natverse.org/fafbseg/reference/flywire_fetch.md)

## Value

A logical vector of length matching the input. NA/0 input values will
return NA as output.

## Details

This call is quite fast (think thousands of ids per second). The current
implementation also de-duplicates the input automatically. You can pass
in a vector containing duplicates and only the unique ids will be passed
on to the server.

If you provide input as `integer64` then data will be sent in binary
form to the flywire server. This can have a significant time saving for
large queries (think 10000+).

When a `timestamp` or `version` is provided, only edits up until that
time point will be considered. Note that since August 2022
`flywire_islatest` will return `FALSE` in the case of a rootid that was
not created until after the `timestamp`. Formerly it returned `TRUE`
(see
[seung-lab/PyChunkedGraph#412](https://github.com/seung-lab/PyChunkedGraph/pull/412))

## See also

Other flywire-ids:
[`flywire_last_modified()`](https://natverse.org/fafbseg/reference/flywire_last_modified.md),
[`flywire_latestid()`](https://natverse.org/fafbseg/reference/flywire_latestid.md),
[`flywire_leaves()`](https://natverse.org/fafbseg/reference/flywire_leaves.md),
[`flywire_rootid()`](https://natverse.org/fafbseg/reference/flywire_rootid.md),
[`flywire_updateids()`](https://natverse.org/fafbseg/reference/flywire_updateids.md),
[`flywire_xyz2id()`](https://natverse.org/fafbseg/reference/flywire_xyz2id.md)

## Examples

``` r
# \donttest{
flywire_islatest("720575940621039145")
#> [1] FALSE
flywire_islatest(c("720575940619073968", "720575940637707136"))
#> [1] FALSE FALSE

# check the first id up to a given timestamp
# when was it created (= last modified)
flywire_last_modified('720575940619073968')
#> [1] "2020-12-02 11:43:01 UTC"
# TRUE
flywire_islatest('720575940619073968', timestamp = '2020-12-03 UTC')
#> [1] TRUE
# FALSE since it didn't exist
flywire_islatest("720575940619073968", timestamp = "2020-12-01 UTC")
#> [1] FALSE
# }
if (FALSE) { # \dontrun{
latest=flywire_latestid("720575940619073968")
flywire_islatest(latest)

# compare checking roots downstream of two large bilateral neurons
blids=c("720575940619073968", "720575940637707136")
blidsout=flywire_partners(blids)
# 3.2 vs 4.7s in my test
bench::mark(bin=flywire_islatest(blidsout$post_id),
  str=flywire_islatest(as.character(blidsout$post_id)))
} # }
```
