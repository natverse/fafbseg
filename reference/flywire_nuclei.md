# Queries for information about flywire nuclei (helpful for finding somata)

`flywire_nuclei` finds nuclei based on known `rootids` or `nucleus_ids`

`flywire_nearest_nuclei` returns the nearest nucleus to a query xyz
location. When `rawcoords=T` both the input and output positions are in
raw voxels. Note however that distances are still calculated in nm.
`xyz` may contain single points unless `k>1`, in which case only one
query point is allowed.

## Usage

``` r
flywire_nuclei(rootids = NULL, nucleus_ids = NULL, rawcoords = FALSE, ...)

flywire_nearest_nuclei(xyz, rawcoords = F, k = 1)
```

## Arguments

- rootids:

  Character vector specifying one or more flywire rootids. As a
  convenience for `flywire_partner_summary` this argument is passed to
  [`flywire_ids`](https://natverse.org/fafbseg/reference/flywire_ids.md)
  allowing you to pass in data.frames, flywire URLs or cell type
  queries.

- nucleus_ids:

  ids from the nucleus table to return (optional, NB only one of
  `rootids` and `nucleus_ids` can be provided).

- rawcoords:

  Whether to return coordinates in raw form rather than nm (default
  `FALSE`)

- ...:

  Additional arguments passed to
  [`flywire_cave_query`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)

- xyz:

  One or more (if `k=1`) query points. In raw coordinates when
  `rawcoords=T`

- k:

  The number of nearest nuclei to return for each query position. When
  `k>1` you are currently limited to one query point.

## Value

A data.frame containing information about nuclei including

- id nucleus id

- pt_position the XYZ position of the centre of the nucleus. This will
  always be in nm when `rawcoords=FALSE` even if the remote table stores
  raw (uncalibrated) voxel coordinates. It will be a comma separated
  string when rawcoords=TRUE since this is most convenient for pasting
  between applications.

- pt_supervoxel_id corresponding to the nucleus may be missing if the
  segmentation is disrupted at the location of the nucleus e.g. because
  of masking issues, missing sections etc.

- pt_root_id The current root id (when the `pt_position` maps onto the
  segmentation.)

- volume the volume in cubic microns of the nucleus

For `flywire_nearest_nuclei` when `rawcoords=T` both the input and
output positions are in raw voxels. Note however that distances are
still calculated in nm.

## Details

`flywire_nearest_nuclei` caches the nucleus table and then updates ids
of any selected values. This saves time for subsequent queries assuming
that you are returning less than half of the total rows.

## Examples

``` r
# \donttest{
# an example where there are two nucleus matches
flywire_nuclei(flywire_xyz2id(c(120152, 22864, 3564), rawcoords = TRUE))
#> # A tibble: 3 × 10
#>        id created             superceded_id valid volume pt_supervoxel_id
#>     <int> <dttm>                      <int> <lgl>  <dbl>          <int64>
#> 1 4390074 2021-06-23 19:56:08            NA TRUE   0.172             7e16
#> 2 3533175 2021-06-23 20:01:43            NA TRUE  21.0               7e16
#> 3 3533210 2021-06-23 19:57:49            NA TRUE   6.74              7e16
#> # ℹ 4 more variables: pt_root_id <int64>, pt_position <list<integer>>,
#> #   bb_start_position <list<integer>>, bb_end_position <list<integer>>
# }
# \donttest{
nn=flywire_nearest_nuclei(c(480608, 91456, 142560), k=2)
as.data.frame(nn)
#>        id             created superceded_id valid    volume  pt_supervoxel_id
#> 1 3533210 2021-06-23 19:57:49            NA  TRUE  6.735708 78884462138180472
#> 2 3533175 2021-06-23 20:01:43            NA  TRUE 20.972258 78884462138083227
#>           pt_root_id           pt_position     bb_start_position
#> 1 720575940624943800 480608, 91456, 142560 478432, 89952, 142120
#> 2 720575940624943800 480512, 90784, 140360 478176, 89184, 138720
#>         bb_end_position     dist
#> 1 482784, 92736, 143440    0.000
#> 2 482944, 92608, 141480 2302.347

flywire_nearest_nuclei('163113, 59074, 5295', rawcoords = TRUE)
#> # A tibble: 1 × 11
#>        id created             superceded_id valid volume pt_supervoxel_id
#>     <int> <dttm>                      <int> <lgl>  <dbl>          <int64>
#> 1 6865768 2021-06-23 20:01:53            NA TRUE    21.7             8e16
#> # ℹ 5 more variables: pt_root_id <int64>, pt_position <chr>,
#> #   bb_start_position <chr>, bb_end_position <chr>, dist <dbl>
# }

if (FALSE) { # \dontrun{
# from clipboard e.g. copied from flywire
flywire_nearest_nuclei(clipr::read_clip(), rawcoords = TRUE)
} # }
```
