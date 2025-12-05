# Access precomputed flywire connectivity data

`flywire_connectome_data` returns a
[`dplyr`](https://dplyr.tidyverse.org/reference/dplyr-package.html)
compatible connection to connectivity dumps on disk.

`flywire_connectome_data_version` sets the integer version number of the
preferred flywire connectome data dump or returns the currently version.

## Usage

``` r
flywire_connectome_data(
  type = c("syn", "pre", "post"),
  version = NULL,
  cached = TRUE,
  ...
)

flywire_connectome_data_version(set = NULL, default = NA)
```

## Arguments

- type:

  Character vector specifying the kind of data

- version:

  Optional CAVE version. The default value of `NULL` uses the latest
  data dump available unless
  `options(fafbseg.flywire_connectome_data_version)` has been set (which
  you can conveniently do using `flywire_connectome_data_version()`).
  The special version of `"783.2"` will use the 2025 (Princeton) synapse
  data release.

- cached:

  When version is `NULL` whether to use a cached value (lasting 1 hour)
  of the latest available version.

- ...:

  Additional arguments passed to
  [`arrow::open_dataset`](https://arrow.apache.org/docs/r/reference/open_dataset.html).

- set:

  When `set=<number>` is passed as an argument the specified data
  version will be used going forwards in this session as the default.
  This is achieved by setting the
  `fafbseg.flywire_connectome_data_version` option. When `set=NA` is
  specified then the option is cleared. When `set=FALSE`, the latest
  version on disk will be returned regardless of the value of
  `options("fafbseg.flywire_connectome_data_version"))`. See examples.

- default:

  A version to return when no other information is available. Defaults
  to `NA` to indicate no version information available.

## Value

An arrow object that you can use with `dplyr` verbs like `filter` in
order to find neurons/connectivity data of interest.

An integer version number *or* a list with the previous value of
`options(fafbseg.flywire_connectome_data_version)` when `set=<number>`.

## Details

This depends on precomputed data dumps prepared periodically by Sven
Dorkenwald. You can download the public release version using the
function
[`download_flywire_release_data`](https://natverse.org/fafbseg/reference/download_flywire_release_data.md).

You can download other versions from Sven's Google drive folder. See
[this FlyWire Slack
message](https://flywire-forum.slack.com/archives/C01M4LP2Y2D/p1644529750249139)
for more details and the URL.

Two pieces of information are used to determine the *version* when it is
queried. First the value of
`options(fafbseg.flywire_connectome_data_version)`, second the latest
available version of the connectivity dumps provided by
`flywire_connectome_data()`.

## See also

[`download_flywire_release_data`](https://natverse.org/fafbseg/reference/download_flywire_release_data.md)

## Examples

``` r
# \donttest{
# latest available version/
syn=try(flywire_connectome_data('syn'), silent=TRUE)
syn450=try(flywire_connectome_data('syn', version=450), silent=TRUE)
if(!inherits(syn450, 'try-error')) {
syn450
syn450$metadata

dl4ds <- syn450 %>%
  filter(pre_pt_root_id==flywire_ids("DL4_adPN_L", version=450, integer64 = TRUE)) %>%
  collect()
}

# }
if (FALSE) { # \dontrun{
# report active connectome dump version (defaults to most recent available)
flywire_connectome_data_version()

# use the June 2023 public release version as the default
flywire_connectome_data_version(set=630)
# confirm this is the default
flywire_connectome_data_version()

# check the latest version on disk
flywire_connectome_data_version(set=FALSE)

# stop defaulting to specific version (therefore using the latest on disk)
flywire_connectome_data_version(set=NA)
flywire_connectome_data_version()
} # }
```
