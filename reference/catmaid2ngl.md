# Convert CATMAID things (eg neurons) into Neuroglancer equivalents

`catmaid2ngl.neuron` uses
[`brainmaps_xyz2id`](https://natverse.org/fafbseg/reference/brainmaps_xyz2id.md)
and
[`read_segments2`](https://natverse.org/fafbseg/reference/read_segments.md)
to find the skeletons corresponding to a (catmaid) neuron

`catmaid2ngl.neuronlist` applies `catmaid2ngl.neuron` over a
[`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html)

`catmaid2ngl.character` uses
[`open_fafb_ngl`](https://natverse.org/fafbseg/reference/open_fafb_ngl.md)
to open CATMAID at a location matching a Neuroglancer URL.

Converts a CATMAID skid specification (see
[`catmaid_skids`](https://rdrr.io/pkg/catmaid/man/catmaid_skids.html))
for one neuron into a an autosegmentation based neuron using
`catmaid2ngl.neuron`.

## Usage

``` r
catmaid2ngl(x, ...)

# S3 method for class 'neuron'
catmaid2ngl(
  x,
  chunksize = getOption("fafbseg.brainmaps_xyz2id.chunksize", 4000),
  ...
)

# S3 method for class 'neuronlist'
catmaid2ngl(x, OmitFailures = TRUE, ...)

# S3 method for class 'character'
catmaid2ngl(x, open = FALSE, ...)

# Default S3 method
catmaid2ngl(x, ...)
```

## Arguments

- x:

  An object to convert (see Descriptions for each method)

- ...:

  Additional arguments passed to methods

- chunksize:

  send queries in batches each of which has at most `chunksize` points.
  The default is chosen since the brainmaps API can time out if the
  points take too long to map (more likely if they are spread out across
  the brain). There is also a maximum number of points per call (10,000
  was the recommended upper limit at one point).

- OmitFailures:

  Whether to omit neurons for which `FUN` gives an error. The default
  value (`NA`) will result in nlapply stopping with an error message the
  moment there is an error. For other values, see details.

- open:

  Whether or not to open the URL in a browser - this defaults to `TRUE`
  in interactive use.
