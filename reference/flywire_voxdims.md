# Handle raw and nm calibrated flywire coordinates

`flywire_voxdims` returns the image voxel dimensions which are normally
used to scale between **raw** and **nm** coordinates.

## Usage

``` r
flywire_voxdims(url = getOption("fafbseg.sampleurl"))

flywire_nm2raw(x, vd = flywire_voxdims())

flywire_raw2nm(x, vd = flywire_voxdims())
```

## Arguments

- url:

  Optional neuroglancer URL containing voxel size. Defaults to
  `getOption("fafbseg.sampleurl")` as set by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md).

- x:

  3D coordinates in any form compatible with
  [`xyzmatrix`](https://natverse.org/fafbseg/reference/xyzmatrix.md)

- vd:

  The voxel dimensions in nm. Expert use only. Normally found
  automatically.

## Value

For `flywire_voxdims` A 3-vector

for `flywire_raw2nm` and `flywire_nm2raw` an Nx3 matrix of coordinates

## Details

relies on nat \>= 1.10.4

## Examples

``` r
flywire_voxdims()
#> [1]  4  4 40
#> attr(,"units")
#> [1] "nm"
# ensure that we use default production flywire scene
with_segmentation('flywire', flywire_voxdims())
#> [1]  4  4 40
#> attr(,"units")
#> [1] "nm"
flywire_raw2nm(c(159144, 22192, 3560))
#>           X     Y      Z
#> [1,] 636576 88768 142400
flywire_raw2nm('159144 22192 3560')
#>           X     Y      Z
#> [1,] 636576 88768 142400
if (FALSE) { # \dontrun{
flywire_nm2raw(clipr::read_clip())
} # }
```
