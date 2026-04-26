# Find FlyWire root or supervoxel (leaf) ids for XYZ locations

Find FlyWire root or supervoxel (leaf) ids for XYZ locations

## Usage

``` r
flywire_xyz2id(
  xyz,
  rawcoords = FALSE,
  voxdims = flywire_voxdims(),
  cloudvolume.url = NULL,
  root = TRUE,
  timestamp = NULL,
  version = NULL,
  stop_layer = NULL,
  integer64 = FALSE,
  fast_root = TRUE,
  method = c("auto", "cloudvolume", "spine"),
  ...
)
```

## Arguments

- xyz:

  One or more xyz locations as an Nx3 matrix or in any form compatible
  with
  [`xyzmatrix`](https://natverse.org/fafbseg/reference/xyzmatrix.md)
  including `neuron` or `mesh3d` surface objects.

- rawcoords:

  whether the input values are raw voxel indices or in nm

- voxdims:

  voxel dimensions in nm used to convert raw coordinates. The default
  value uses the
  [`flywire_voxdims`](https://natverse.org/fafbseg/reference/flywire_voxdims.md)
  function to identify the value for the current segmentation (usually
  with success).

- cloudvolume.url:

  URL for CloudVolume to fetch segmentation image data. The default
  value of NULL chooses the flywire production segmentation dataset.

- root:

  Whether to return the root id of the whole segment rather than the
  supervoxel id.

- timestamp:

  An optional timestamp as a string or POSIXct, interpreted as UTC when
  no timezone is specified.

- version:

  An optional CAVE materialisation version number. See details and
  examples.

- stop_layer:

  Which layer of the chunked graph to stop at. The default `NULL` is
  equivalent to layer 1 or the full root id. Coarser layer 2 IDs can be
  a useful intermediate for some operations.

- integer64:

  Whether to return ids as integer64 type (more compact but a little
  fragile) rather than character (default `FALSE`).

- fast_root:

  Whether to use a fast but two-step look-up procedure when finding
  roots. This is strongly recommended and the alternative approach has
  only been retained for validation purposes.

- method:

  Whether to use the [spine
  transform-service](https://services.itanna.io/app/transform-service/docs)
  API or cloudvolume for lookup. `"auto"` is presently a synonym for
  `"spine"`.

- ...:

  additional arguments passed to `pbapply` when looking up multiple
  positions.

## Value

A character vector of segment ids, `NA` when lookup fails.

## Details

root ids define a whole neuron or segmented object. supervoxel ids
correspond to a small group of voxels that it is assumed must all belong
to the same object. supervoxel ids do not change for a given a
segmentation, whereas root ids change every time a neuron is edited. The
most stable way to refer to a FlyWire neuron is to choose a nice safe
location on the arbour (I recommend a major branch point) and then store
the location and supervoxel id. You can rapidly map the supervoxel id to
the current root id using
[`flywire_rootid`](https://natverse.org/fafbseg/reference/flywire_rootid.md).

As of November 2020, the default approach to look up supervoxel ids for
a 3D point is using the [spine
transform-service](https://services.itanna.io/app/transform-service/docs)
API. This is order 100x faster than mapping via cloudvolume (since that
must make a single web request for every point) and Eric Perlman has
optimised the layout of the underlying data for rapid mapping.

Note when using the slower cloudvolume method (which may still be
required for volumes for which a fast lookup via spine is not available)
that finding the supervoxel for a given XYZ location is order 3x faster
than finding the root id for the agglomeration of all of the super
voxels in a given object. Perhaps less intuitively, if you want to look
up many root ids, it is actually quicker to do `flywire_xyz2id(,root=F)`
followed by
[`flywire_rootid`](https://natverse.org/fafbseg/reference/flywire_rootid.md)
since that function can look up many root ids in a single call to the
ChunkedGraph server. This is what happens when setting `fast_root=TRUE`,
the default.

## See also

Other flywire-ids:
[`flywire_islatest()`](https://natverse.org/fafbseg/reference/flywire_islatest.md),
[`flywire_last_modified()`](https://natverse.org/fafbseg/reference/flywire_last_modified.md),
[`flywire_latestid()`](https://natverse.org/fafbseg/reference/flywire_latestid.md),
[`flywire_leaves()`](https://natverse.org/fafbseg/reference/flywire_leaves.md),
[`flywire_rootid()`](https://natverse.org/fafbseg/reference/flywire_rootid.md),
[`flywire_updateids()`](https://natverse.org/fafbseg/reference/flywire_updateids.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# example based on defining some sample points from real neurons
# sample dataset from FAFB catmaid
n=elmr::dense_core_neurons[[1]]
set.seed(42)
ss=sample(nvertices(n), size=10)
# first convert FAF14 points to FlyWire coordinate space
nx=xform_brain(elmr::dense_core_neurons[[1]], ref="FlyWire", sample="FAFB14")
pts=xyzmatrix(nx)[ss,]
} # }

# for simplicity just define those same sample points directly
pts = matrix(
c(428910.52, 110629.64, 174800, 410201.86, 110419.1, 180400,
  337136, 129926.28, 189280, 349981.85, 136041.53, 199280, 398361.74,
  110731.26, 182640, 382789.26, 118987.04, 189920, 358033.92, 171592.92,
  143960, 360990.48, 140277.42, 201400, 376258.06, 125201.51, 194400,
  331370.31, 128338.58, 181760),
ncol = 3, byrow = TRUE,
dimnames = list(NULL, c("X", "Y", "Z"))
)

#' # Fast and simple appoach to find ids in one (user-facing) step
flywire_xyz2id(pts)
#>  [1] "720575940634984800" "720575940634984800" "720575940634984800"
#>  [4] "720575940634984800" "720575940634984800" "720575940634984800"
#>  [7] "720575940634984800" "720575940634984800" "720575940634984800"
#> [10] "720575940634984800"


## illustrate what's happening under the hood

# find the ids for the selected xyz locations
# NB fast_root=FALSE was the default behaviour until Nov 2020
flywire_xyz2id(pts, fast_root=FALSE, method="cloudvolume")
#>  [1] "720575940634984800" "720575940634984800" "720575940634984800"
#>  [4] "720575940634984800" "720575940634984800" "720575940634984800"
#>  [7] "720575940634984800" "720575940634984800" "720575940634984800"
#> [10] "720575940634984800"

# we can also find the supervoxels - much faster
svids=flywire_xyz2id(pts, root=FALSE)

# now look up the root ids - very fast with method="cloudvolume", the default
flywire_rootid(svids)
#>  [1] "720575940634984800" "720575940634984800" "720575940634984800"
#>  [4] "720575940634984800" "720575940634984800" "720575940634984800"
#>  [7] "720575940634984800" "720575940634984800" "720575940634984800"
#> [10] "720575940634984800"
# }
```
