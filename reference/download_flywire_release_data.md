# Download FlyWire connectivity and annotations from public release

Download FlyWire connectivity and annotations from public release

## Usage

``` r
download_flywire_release_data(
  which = c("core", "all"),
  version = c(783L, 630L)
)
```

## Arguments

- which:

  Which data to download. `core` gets the most used files (~300 MB).
  `all` gets some additional useful ones (~900 MB).

- version:

  Which materialisation version to use. See details.

## Value

No return value - just used for its side effect of downloading files.

## Details

Note that you must accept to abide by the flywire principles in order to
use flywire data.

Version 630 released with the June 2023 bioRxiv manuscripts remains the
default for the time being but there are significant improvements in the
cell typing associated with version 783 which should be released with
the Dec 2023 resubmissions of the core flywire manuscripts.

## See also

[`flywire_connectome_data`](https://natverse.org/fafbseg/reference/flywire_connectome_data.md),
[`flywire_partner_summary2`](https://natverse.org/fafbseg/reference/flywire_partner_summary2.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# 300 MB
download_flywire_release_data()
# 900 MB includes
download_flywire_release_data('all')
} # }
```
