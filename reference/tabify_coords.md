# Convert coordinates to tab separated values. Useful for copy/paste to Seatable

`tabify_coords` can be used to convert a comma separated value on the
clipboard e.g. from neuroglancer to tab separated values needed by
Seatable when coordinates are stored in 3 separate columns. It also
words for multiple coordinates.

## Usage

``` r
tabify_coords(xyz = NULL, FUN = NULL, write_clip = NULL)
```

## Arguments

- xyz:

  3D Coordinates in any form compatible with
  [`xyzmatrix`](https://natverse.org/fafbseg/reference/xyzmatrix.md).
  When missing these are read from the clipboard.

- FUN:

  a transformer function to apply to the incoming coordinates. As a
  convenience if `FUN` is missing and `xyz` is a function then that
  function will be applied to coordinates on the clipboard.

- write_clip:

  Whether to write the result to the clipboard. When missing (the
  default) will write to clipboard only if coordinates were read from
  the clipboard because `xyz=NULL`.

## Value

Character vector tab separated coordinates. When `xyz` is missing these
will be returned invisibly and also written to the clipboard.

## Examples

``` r
# \donttest{
tabify_coords(1:3)
#> [1] "1\t2\t3"
# }
if (FALSE) { # \dontrun{
# copy position from clipboard and write back as TSV
tabify_coords()
# same but convert from raw coordinates to nm
tabify_coords(flywire_raw2nm)
} # }
```
