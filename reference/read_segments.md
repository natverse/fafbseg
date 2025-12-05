# Read skeletons for segments by extracting from the corresponding zip file(s)

`read_segments2` is a reworked version of `read_segments` that reads
skeletons straight from zip files to memory.

## Usage

``` r
read_segments(x, voxdims = c(32, 32, 40), ...)

read_segments2(
  x,
  voxdims = c(32, 32, 40),
  minfilesize = 80,
  datafrac = NULL,
  coordsonly = FALSE,
  ...
)
```

## Arguments

- x:

  A vector of segment ids or any Neuroglancer scene specification that
  includes segments ids (see examples and
  [`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  for details).

- voxdims:

  The voxel dimensions in nm of the skeletonised data

- ...:

  additional arguments passed to
  [`read.neurons`](https://rdrr.io/pkg/nat/man/read.neurons.html)

- minfilesize:

  The uncompressed size of the swc file must be \>= this. A cheap way to
  insist that we have \>1 point.

- datafrac:

  Fraction of the data to read based on uncompressed file size (see
  details)

- coordsonly:

  Only read in XYZ coordinates of neurons.

## Value

A [`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) containing
one [`neuron`](https://rdrr.io/pkg/nat/man/neuron.html) for each
fragment

## Details

I would recommend `read_segments2` at this point. `read_segments` has
the potential benefit of caching SWC files on disk rather than
extracting every time. However there is a large slowdown on many
filesystems as the number of extracted files enters the thousands -
something that I have hit a few times. Furthermore `read_segments2`
makes it easier to select fragment files *before* extracting them.

`datafrac` a number in the range 0-1 specifies a fraction of the data to
read. Skeleton fragments will be placed in descending size order and
read in until the number of bytes exceeds `datafrac` \* sum(all file
sizes). We have noticed that the time taken to read a neuron from a zip
file seems to depend largely on the number of fragments that are read
in, rather than the amount of data in each fragment! Reading 90 can take
\< 10

## See also

[`read.neurons`](https://rdrr.io/pkg/nat/man/read.neurons.html),
[`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md),
[`read_brainmaps_meshes`](https://natverse.org/fafbseg/reference/read_brainmaps_meshes.md)
to read 3D meshes.

## Examples

``` r
if (FALSE) { # \dontrun{
# read neuron using raw segment identifier
n <- read_segments2(22427007374)

# read a neuron from a scene specification copied from Neuroglancer window
# after clicking on the {} icon at top right
n <- read_segments2(clipr::read_clip())

summary(n)

n2 <- read_segments2(22427007374, datafrac=0.9)
summary(n2)
} # }
```
