# Low level function to fetch skeleton from brainmaps API

Low level function to fetch skeleton from brainmaps API

## Usage

``` r
brainmaps_skeleton(x, skeletonuri = getOption("fafbseg.skeletonuri"), ...)
```

## Arguments

- x:

  A single segment id

- skeletonuri:

  The brainmaps URI describing the skeleton source. Defaults to the
  value of `options("fafbseg.skeletonuri")`.

- ...:

  Additional arguments passed to
  [`brainmaps_fetch`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md)

## Value

A list containing the following fields

- `nvertices` The number of vertices (n)

- `nedges` The number of edges (m)

- `vertices` A `n` x 3 matrix of vertex locations

## Details

This API seems to return skeletons in physical (i.e. already calibrated)
coordinates.

## Examples

``` r
if (FALSE) { # \dontrun{
brainmaps_skeleton(9208128833)
brainmaps_skeleton(9208128833,
  skeletonuri=paste0("brainmaps://772153499790:fafb_v14:",
  "fafb_v14_16nm_v00c_split3xfill2_flatreseg2_skeletons32",
  "/teasar512_nnconn165_mc10000_prune10_thresh1000_sparse250"))
} # }
```
