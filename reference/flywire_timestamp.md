# Find standard UTC timestamp for flywire materialisation version or timestamp

Find standard UTC timestamp for flywire materialisation version or
timestamp

## Usage

``` r
flywire_timestamp(
  version = NULL,
  timestamp = NULL,
  convert = TRUE,
  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")
)
```

## Arguments

- version:

  Integer materialisation version. The special value of `'latest'` means
  the most recent materialisation according to CAVE.

- timestamp:

  A timestamp to normalise into an R or Python timestamp in UTC. The
  special value of `'now'` means the current time in UTC.

- convert:

  Whether to convert from Python to R timestamp (default: `TRUE`)

- datastack_name:

  defaults to the value selected by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  and to "flywire_fafb_production" when that is missing. See
  <https://global.daf-apis.com/info/> for other options.

## Value

A POSIXct object or Python datetime object in the UTC timezone.

## Details

Note that all CAVE timestamps are in UTC. When the `timestamp` argument
is a character vector **it is assumed to be in UTC regardless of any
timezone specification**. Unless the input character vector contains the
string "UTC" then a warning will be issued.

## See also

Other cave-queries:
[`flywire_cave_query()`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)

## Examples

``` r
# \donttest{
ts=flywire_timestamp(349)
ts
#> [1] "2022-04-18 08:10:00 UTC"
# As a unix timestamp (number of seconds since 00:00 on 1970-01-01)
as.numeric(ts)
#> [1] 1650269400
tsp=flywire_timestamp(349, convert=FALSE)
# should be same as the numeric timestamp above
tsp$timestamp()
#> [1] 1650269400

flywire_timestamp(timestamp="2022-08-28 17:04:49 UTC")
#> [1] "2022-08-28 17:04:49 UTC"

# nb this will return the current time *in UTC* regardless of your timezone
flywire_timestamp(timestamp="now")
#> [1] "2026-02-17 14:42:38 UTC"
# }
if (FALSE) { # \dontrun{
# same but gives a warning
flywire_timestamp(timestamp="2022-08-28 17:04:49")
} # }
```
