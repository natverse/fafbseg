# Read 3D meshes via the brainmaps API

Read 3D meshes via the brainmaps API

## Usage

``` r
read_brainmaps_meshes(
  x,
  volume = getOption("fafbseg.brainmaps.volume"),
  meshName = getOption("fafbseg.brainmaps.meshName"),
  ...
)
```

## Arguments

- x:

  Vector of integer segment ids

- volume:

  String identifier for the volume containing segments

- meshName:

  String identifier for the meshes

- ...:

  Additional arguments passed to
  [`brainmaps_fetch`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md)

## Value

A [`mesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
object

## See also

[`read_segments2`](https://natverse.org/fafbseg/reference/read_segments.md)
to read skeleton fragments and
[`brainmaps_listfragments`](https://natverse.org/fafbseg/reference/brainmaps_listfragments.md)
(to identify the fragments that must be read). See
[`compare_ng_neuron`](https://natverse.org/fafbseg/reference/compare_ng_neuron.md)
to compare skeletons and 3D meshes.

## Examples

``` r
if (FALSE) { # \dontrun{
segs=find_merged_segments(7186840767)
samplemesh=read_brainmaps_meshes(segs)
sampleskel=read_segments2(segs)
dot3d(samplemesh, col='grey')
plot3d(sampleskel, lwd=2)

# or compare mesh and skeleton colouring points by distance from skeleton
compare_ng_neuron(samplemesh, sampleskel, pointsize=1, sample_dots = 0.3)
} # }
```
