# Extract brainmaps volume identifier

Extract brainmaps volume identifier

## Usage

``` r
brainmaps_volume(x)
```

## Arguments

- x:

  character vector containing brainmaps URI, a parsed `brainmaps_uri`
  object or a character vector already containing a volume specifier.

## Value

character vector containing a volume specifier.

## Examples

``` r
brainmaps_volume("772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2")
#> [1] "772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2"
brainmaps_volume("brainmaps://772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2")
#> [1] "772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2"
```
