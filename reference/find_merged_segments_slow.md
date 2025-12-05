# Find all merged segment ids for a given starting google segment id

This is a naive implementation that only depends only on the `mergeinfo`
object created by
[`read_mergeinfo`](https://natverse.org/fafbseg/reference/read_mergeinfo.md).
It is retained only to verify correctness. It is slow but does not
require any pre-processing of the table of merge information.

## Usage

``` r
find_merged_segments_slow(x, mergeinfo)
```

## Arguments

- x:

  a segment id or any other input that can be interpreted by
  [`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md)

- mergeinfo:

  The merge information data.frame created by
  [`read_mergeinfo`](https://natverse.org/fafbseg/reference/read_mergeinfo.md)

## Value

vector of segment ids

## See also

[`find_merged_segments`](https://natverse.org/fafbseg/reference/find_merged_segments.md)
