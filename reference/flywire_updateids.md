# Update flywire root ids to any timestamp using XYZ position or supervoxel ids

`flywire_updateids` updates root ids to the latest version (or any
arbitrary integer materialisation version / timestamp that you specify).
To be as efficient as possible, it will use supervoxel ids, XYZ
positions or failing that the slower
[`flywire_latestid`](https://natverse.org/fafbseg/reference/flywire_latestid.md).

As of November 2022 the default behaviour will use a per-session
supervoxel id to root id cache in order to speed up repeated lookups for
the same root id / supervoxel id pairs at a given timestamp/version. See
[`flywire_rootid`](https://natverse.org/fafbseg/reference/flywire_rootid.md)
for further details.

## Usage

``` r
flywire_updateids(
  x,
  svids = NULL,
  xyz = NULL,
  rawcoords = FALSE,
  voxdims = c(4, 4, 40),
  cache = NA,
  version = NULL,
  timestamp = NULL,
  Verbose = TRUE,
  ...
)
```

## Arguments

- x:

  Current root ids

- svids:

  optional supervoxel ids

- xyz:

  optional xyz locations in any form understood by
  [`xyzmatrix`](https://natverse.org/fafbseg/reference/xyzmatrix.md)

- rawcoords:

  whether the input values are raw voxel indices or in nm

- voxdims:

  voxel dimensions in nm used to convert raw coordinates. The default
  value uses the
  [`flywire_voxdims`](https://natverse.org/fafbseg/reference/flywire_voxdims.md)
  function to identify the value for the current segmentation (usually
  with success).

- cache:

  Whether to cache supervoxel id to root id mappings. The default value
  of `NA` will do this when a version or timestamp argument is
  specified. See
  [`flywire_rootid`](https://natverse.org/fafbseg/reference/flywire_rootid.md)
  for details.

- version:

  An optional CAVE materialisation version number. See details and
  examples.

- timestamp:

  An optional timestamp as a string or POSIXct, interpreted as UTC when
  no timezone is specified.

- Verbose:

  Whether to print a message to the console when updates are required.

- ...:

  Additional arguments passed to
  [`flywire_islatest`](https://natverse.org/fafbseg/reference/flywire_islatest.md)
  or
  [`flywire_latestid`](https://natverse.org/fafbseg/reference/flywire_latestid.md)

## Value

A vector of the same form as `x` with updated ids.

## See also

Other flywire-ids:
[`flywire_islatest()`](https://natverse.org/fafbseg/reference/flywire_islatest.md),
[`flywire_last_modified()`](https://natverse.org/fafbseg/reference/flywire_last_modified.md),
[`flywire_latestid()`](https://natverse.org/fafbseg/reference/flywire_latestid.md),
[`flywire_leaves()`](https://natverse.org/fafbseg/reference/flywire_leaves.md),
[`flywire_rootid()`](https://natverse.org/fafbseg/reference/flywire_rootid.md),
[`flywire_xyz2id()`](https://natverse.org/fafbseg/reference/flywire_xyz2id.md)

## Examples

``` r
kcs=data.frame(
rootid=c("720575940602553568", "720575940602564320", "720575940602605536"),
xyz=c("(159284,42762,3594)", "(159035,41959,3594)", "(157715,44345,3594)")
)
# update root ids
kcs$rootid=flywire_updateids(kcs$rootid, xyz=kcs$xyz)
```
