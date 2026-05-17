# Sum level 2 volumes for one or more FlyWire root ids

Sum level 2 volumes for one or more FlyWire root ids

## Usage

``` r
flywire_l2volume(
  rootids,
  cache = TRUE,
  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
  ...
)
```

## Arguments

- rootids:

  One or more FlyWire root ids.

- cache:

  Whether to cache rootid-level volume totals on disk.

- datastack_name:

  defaults to the value selected by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  and to "flywire_fafb_production" when that is missing. See
  <https://global.daf-apis.com/info/> for other options.

- ...:

  Additional arguments passed to
  [`flywire_l2attributes`](https://natverse.org/fafbseg/reference/flywire_l2attributes.md)
  and to recursive calls when `rootids` has length greater than one.

## Value

A numeric scalar volume in nm^3 for one input root id, or a named vector
when `rootids` has length greater than one.
