# Convert between filenames and neuroglancer ids

`swc2segmentid` converts an swc filename to a segment id

`segmentid2zip` converts a segment id to the zip file that contains it

`zip2segmentstem` converts a zip file to the initial part of the segment
id i.e. the segment stem (see details).

## Usage

``` r
swc2segmentid(x, include.fragment = FALSE)

segmentid2zip(x)

zip2segmentstem(x)
```

## Arguments

- x:

  Input file or id

- include.fragment:

  Whether to include the sub identifier of the skeleton fragment (see
  details).

## Value

for `swc2segmentid` a numeric vector or matrix depending on the value of
`include.fragment`

## Details

Segment ids are unique integers. There are about 8E8 in the current
skeletonisation but it seems that the ids can still be \> 2^31 (usually
`.Machine$integer.max`). Therefore they will be stored in R as numeric
values or the
[`bit64::integer64`](https://rdrr.io/pkg/bit64/man/bit64-package.html)
values.

Each segmentation has keen skeletonised however this usually results in
multiple skeleton fragments which have been written out as separate SWC
files: `"named <segment id>.<fragment>.swc"`

Each segment id is mapped onto a zip file by dividing by a divisor and
discarding the remainder. Peter Li's data release of 2018-10-02 switched
from 1E5 to 1E6.

## Examples

``` r
swc2segmentid("10001654273.1.swc")
#> [1] 10001654273
swc2segmentid(sprintf("10001654273.%d.swc", 0:2), include.fragment=TRUE)
#>          segment fragment
#> [1,] 10001654273        0
#> [2,] 10001654273        1
#> [3,] 10001654273        2
if (FALSE) { # \dontrun{
# NB the default segmentation for fafbseg (flywire) no longer implies a local
# collection of skeletons, wrap calls in with_segmentation
with_segmentation("20190805", segmentid2zip(10001654273))
with_segmentation("20190805",
  segmentid2zip(swc2segmentid("10001654273.1.swc")))
} # }
```
