# Skeletonize neurons using meshparty python library

Skeletonize neurons using meshparty python library

## Usage

``` r
meshparty_skeletonize(segments, savedir = NULL, invalidation_d = 12000, ...)
```

## Arguments

- segments:

  neuron ids in any form understood by
  [`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  OR paths to obj files already saved to disk.

- savedir:

  Where to save SWC files (defaults to temporary directory)

- invalidation_d:

  Distance parameter (nm) controlling skeletonisation level of detail.
  See meshparty docs.

- ...:

  additional arguments passed to `save_cloudvolume_meshes`

## Value

A character vector containing the path to one or more SWC files. Note
that these SWCs will be calibrated in µm even though the input data are
in nm.

## See also

[`simple_python`](https://natverse.org/fafbseg/reference/simple_python.md)
for installation of the necessary Python packages.

## Examples

``` r
if (FALSE) { # \dontrun{
meshparty_skeletonize("720575940614134045")
meshparty_skeletonize("720575940614134045.obj")
} # }
```
