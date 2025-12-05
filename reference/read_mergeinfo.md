# Read CSV files containing raw agglomeration (merge) information

Read CSV files containing raw agglomeration (merge) information

## Usage

``` r
read_mergeinfo(x, pattern = "10\\.csv$", voxdims = c(16, 16, 40), ...)
```

## Arguments

- x:

  Input directory

- pattern:

  Optional pattern to identify CSV files

- voxdims:

  Optional voxel dimensions to convert raw pixel coordinates into
  physical (nm) coordinates

- ...:

  Additional arguments passed to
  [`readr::read_csv`](https://readr.tidyverse.org/reference/read_delim.html)

## Value

a `data.frame` with columns

- `id1,id2` Segment ids to be merged

- `x,y,z` Location (in nm) of merge point

## Details

Peter Li provided 10 CSV files with merge information in Aug 2018
against which this function has been tested. This merge information has
been processed to identify which groups segments should be merged
together in the
[`fafbsegdata`](https://rdrr.io/pkg/fafbsegdata/man/fafbsegdata-package.html)
package, which can be accessed using the
[`find_merged_segments`](https://natverse.org/fafbseg/reference/find_merged_segments.md)
function.

## See also

[`find_merged_segments`](https://natverse.org/fafbseg/reference/find_merged_segments.md),
[`fafbsegdata`](https://rdrr.io/pkg/fafbsegdata/man/fafbsegdata-package.html)
package

## Examples

``` r
if (FALSE) { # \dontrun{
csvdir="~/projects/fafbseg/fafb14_v00c_split3xfill2x_merges/ffnreseg16nm_ms1000_md0.02_c0.6_iou0.7/"
mergeinfo <- read_mergeinfo(csvdir)

# Show 3D position of merge locations for a large merge group containing a
# tangential cell (widefield visual interneuron)
tang_segs <- find_merged_segments(22139217567)
tang_mi <- subset(mergeinfo, id1%in% tang_segs | id2 %in%tang_segs)
points3d(xyzmatrix(tang_mi))

# plot brain surface for context
library(elmr)
plot3d(FAFB)
} # }
```
