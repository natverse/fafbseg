# Check whether an L2 cache is available

Check whether an L2 cache is available

## Usage

``` r
flywire_has_l2cache(
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

Logical scalar indicating whether the current datastack has an
associated level 2 cache.
