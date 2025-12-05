# Compare a neuroglancer mesh object with a regular neuron

Compare a neuroglancer mesh object with a regular neuron

## Usage

``` r
compare_ng_neuron(
  x,
  n,
  breaks = 3,
  colpal = c("cyan", "red"),
  plot = TRUE,
  plotn = plot,
  pointsize = 0.1,
  sample_dots = 1,
  ...
)
```

## Arguments

- x:

  A neuroglancer mesh, rgl::mesh3d object or other object with a defined
  xyzmatrix function.

- n:

  A nat::neuron object

- breaks:

  either a numeric vector of two or more unique cut points or a single
  number (greater than or equal to 2) giving the number of intervals
  into which `x` is to be cut.

- colpal:

  A function defining a colour palette or a vector of colour names.
  Should

- plot:

  Whether to plot anything (set to `FALSE` when you just want to get the
  distance information)

- plotn:

  Whether to plot the neuron `n`

- pointsize:

  Size of plotted points for mesh - passed on to
  [`points3d`](https://dmurdoch.github.io/rgl/dev/reference/primitives.html).
  Default `pointsize=0.1` makes points smaller than usual.

- sample_dots:

  Fraction of points (0-1) from the mesh to plot - the default value of
  1 implies all points. Values of `sample_dots < 1` select a random
  subsample of the points.

- ...:

  Additional arguments passed to `plot3d.neuron`

## Value

Invisibly, a data.frame with the distances of each object in `x` to its
nearest neighbour in `n` as well as the breaks used for colouring points
when plotting.

## See also

[`read_brainmaps_meshes`](https://natverse.org/fafbseg/reference/read_brainmaps_meshes.md)
to read 3D meshes from remote server,
[`read_segments2`](https://natverse.org/fafbseg/reference/read_segments.md)
to read skeletons from zip files, and
[`read.neurons`](https://rdrr.io/pkg/nat/man/read.neurons.html) and
[`read.neurons.catmaid`](https://rdrr.io/pkg/catmaid/man/read.neuron.catmaid.html)
and friends to read regular neurons from disk or a CATMAID server.

## Examples

``` r
if (FALSE) { # \dontrun{
x=read_ng_raw('meshdata/')
library(elmr)
y=read.neuron.catmaid(23432)
compare_ng_neuron(x,y)
} # }
```
