# Return FlyWire level 2 cache metadata

Return FlyWire level 2 cache metadata

## Usage

``` r
flywire_l2cache_metadata(
  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")
)
```

## Arguments

- datastack_name:

  defaults to the value selected by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  and to "flywire_fafb_production" when that is missing. See
  <https://global.daf-apis.com/info/> for other options.

## Value

A named list mapping available level 2 cache attributes to their
declared data types.
