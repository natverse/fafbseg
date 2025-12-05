# Check when a root id was last edited

Check when a root id was last edited

## Usage

``` r
flywire_last_modified(x, tz = "UTC", cloudvolume.url = NULL, ...)
```

## Arguments

- x:

  A set of flywire ids

- tz:

  A timezone in which to display the modification time; defaults to UTC
  (~ the proper name for GMT).

- cloudvolume.url:

  The segmentation source URL for cloudvolume. Normally you can ignore
  this and rely on the default segmentation chosen by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)

- ...:

  additional arguments passed to the low-level
  [`flywire_fetch`](https://natverse.org/fafbseg/reference/flywire_fetch.md)
  function

## Value

A vector of [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html)
timestamps, by default in the standard UTC timezone.

## Details

The raw information for this call is in seconds since the epoch, 00:00
on 1 January 1970 UTC. Specifying a `tz` argument changes the display
but not the actual time of the event i.e. Princeton will typically
display 5h behind Cambridge corresponding to the same physical time.

## See also

Other flywire-ids:
[`flywire_islatest()`](https://natverse.org/fafbseg/reference/flywire_islatest.md),
[`flywire_latestid()`](https://natverse.org/fafbseg/reference/flywire_latestid.md),
[`flywire_leaves()`](https://natverse.org/fafbseg/reference/flywire_leaves.md),
[`flywire_rootid()`](https://natverse.org/fafbseg/reference/flywire_rootid.md),
[`flywire_updateids()`](https://natverse.org/fafbseg/reference/flywire_updateids.md),
[`flywire_xyz2id()`](https://natverse.org/fafbseg/reference/flywire_xyz2id.md)

## Examples

``` r
# \donttest{
flywire_last_modified("720575940639218165")
#> [1] "2021-05-07 09:50:36 UTC"
# Your local time zone
flywire_last_modified("720575940639218165", tz="")
#> [1] "2021-05-07 09:50:36 UTC"
# Cambridge (the original one)
flywire_last_modified("720575940639218165", tz="GB")
#> [1] "2021-05-07 10:50:36 BST"
# Princeton
flywire_last_modified("720575940639218165", tz="US/Eastern")
#> [1] "2021-05-07 05:50:36 EDT"
# }
```
