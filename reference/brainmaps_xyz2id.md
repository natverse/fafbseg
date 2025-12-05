# Convert 3D x,y,z locations in brainmaps volumes to segmentation ids

Convert 3D x,y,z locations in brainmaps volumes to segmentation ids

## Usage

``` r
brainmaps_xyz2id(
  xyz,
  volume = getOption("fafbseg.skeletonuri"),
  rawcoords = FALSE,
  rawvoxdims = c(8, 8, 40),
  chunksize = getOption("fafbseg.brainmaps_xyz2id.chunksize", 200),
  ...
)
```

## Arguments

- xyz:

  N x 3 matrix of points or an object containing vertex data that is
  compatible with
  [`xyzmatrix`](https://natverse.org/fafbseg/reference/xyzmatrix.md).
  These should be in physical space (i.e. nm) unless `voxdims=NULL`.

- volume:

  character vector identifier string for the volume containing
  segmentation data - by default it uses the value of the
  `fafbseg.skeletonuri` option. Any input that can be parsed by
  [`brainmaps_volume`](https://natverse.org/fafbseg/reference/brainmaps_volume.md)
  is acceptable.

- rawcoords:

  Whether the coordinates are voxel indices (when `rawcoords=TRUE`) or
  physical units (nm, the default).

- rawvoxdims:

  the implied voxel dimensions for the volume. If `rawcoords=TRUE` then
  this will be used to convert the raw coordinates into physical units.
  The default value matches the normal voxel size in neuroglancer. If
  `rawvoxdims=NULL` then no attempt is made to scale the coordinates
  whatsoever. See details.

- chunksize:

  send queries in batches each of which has at most `chunksize` points.
  The default is chosen since the brainmaps API can time out if the
  points take too long to map (more likely if they are spread out across
  the brain). There is also a maximum number of points per call (10,000
  was the recommended upper limit at one point).

- ...:

  Additional arguments passed to
  [`brainmaps_fetch`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md).
  This can include setting the `retry` argument to \>0. See **Details**
  and **Examples**.

## Value

A numeric vector of Google segment ids

## Details

The underlying `brainmaps` API expects raw coordinates i.e. voxel
indices. This is slightly complicated by the fact that different
segmentation, skeleton etc volumes may have different associated voxel
dimensions. `brainmaps_xyz2id` automatically looks up this voxel
dimension.

However it may be that you want to pass in raw coordinates from
neuroglancer. These will generally be associated with a the resolution
of the image data not e.g. skeletons which may have a larger (coarser)
voxel size. As a convenience in this situation you can set
`rawcoords=TRUE`.

If you have problems with requests failing sporadically, it may be
helpful to know

- Google does not keep the underlying data live so there may be some
  spin-up time.

- there is an arbitrary timeout at the Google end (currently ~ 5s)

- batching nearby points helps

Using the `chunksize` argument or the `retry` argument passed on to
[`brainmaps_fetch`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md)
can overcome these issues. See the examples.

## Examples

``` r
if (FALSE) { # \dontrun{
# Physical location in nm
brainmaps_xyz2id(c(433368, 168208, 128480))
# Same location as displayed in neuroglancer
brainmaps_xyz2id(c(54171, 21026, 3212), rawcoords=TRUE)

# Raw coodinates for the brainmaps volume in question - don't touch
brainmaps_xyz2id(c(433368, 168208, 128480)/c(32,32,40), rawvoxdims=NULL)

library(elmr)
# get a manually traced neuron (just keep first and only entry in neuronlist)
dl4=read.neurons.catmaid('glomerulus DL4 right')[[1]]
# map every node location to segmentation ids
dl4.segs=brainmaps_xyz2id(dl4)
# remove unmapped locations which get id 0
dl4.segs=setdiff(dl4.segs, 0)
# read in corresponding skeletons
dl4.skels=read_segments2(dl4.segs)
# read in corresponding skeletons after including agglomeration merge groups
dl4.allskels=read_segments2(find_merged_segments(dl4.segs))

## retries / cache / chunksize issues
# set small chunk size
dl4.segs=brainmaps_xyz2id(dl4, chunksize=500)
# use retries in case of failure
dl4.segs=brainmaps_xyz2id(dl4, chunksize=500, retry=3)
# cache successful requests (if you might need to repeat)
dl4.segs=brainmaps_xyz2id(dl4, chunksize=500, retry=3, cache=TRUE)
} # }
```
