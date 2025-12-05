# Find all merged segment ids for a given starting google segment id

Find all merged segment ids for a given starting google segment id

## Usage

``` r
find_merged_segments(
  x,
  return.groups = FALSE,
  return.segmentids.for.groups = TRUE
)
```

## Arguments

- x:

  a segment id or any other input that can be interpreted by
  [`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md)

- return.groups:

  Whether to return the merge groups as well as the segment ids

- return.segmentids.for.groups:

  Whether to return the canonical segment id for each group rather than
  the 1-index of the merge group (see details).

## Value

vector of segment ids (in ascending order) or when `return.groups=TRUE`
a `data.frame` with columns

- `segment` the integer segment id, as a numeric (double) column

- `group` an arbitrary group id starting from 1 OR the canonical segment
  id (see details), an integer or numeric (double), respectively

## Details

segment ids in `ffn16reseg-ms1000_md0.02_c0.6_iou0.7` always match one
raw segment id in `fafb_v14_16nm_v00c_split3xfill2` but may map to a
large agglomerated merge group.

We provide two ways to define an identifier for each of these merge
groups. One uses a canonical segment id: this is the first (i.e. lowest)
segment id in the merge group and appears to be how the brainmaps API
identifies the merge group. This will be the case even when there are no
known merges for a given segment. In R this field will be a numeric
(double) column since R does not have native support for 64 bit
integers.

The second approach to group identifiers uses an integer from 1 to the
number of known merge groups. When there are no known merges for a
segment, an `NA` value will be returned.

Note that this function depends on the
[`segment_merge_groups.dt`](https://rdrr.io/pkg/fafbsegdata/man/segment_merge_groups.dt.html)
object from the
[`fafbsegdata`](https://rdrr.io/pkg/fafbsegdata/man/fafbsegdata-package.html)
package.

## Examples

``` r
# \donttest{
if(requireNamespace('fafbsegdata', quietly = TRUE)) {
find_merged_segments(7186840767)
# these all belong to one merge group by definition
find_merged_segments(7186840767, return.groups=TRUE)
}
#>        segment      group
#> 1  10218295532 7186840767
#> 2  10233257420 7186840767
#> 3   7186840767 7186840767
#> 4   7186844852 7186840767
#> 5   7188488573 7186840767
#> 6   7188488876 7186840767
#> 7   7506547847 7186840767
#> 8   7511521487 7186840767
#> 9   7513174533 7186840767
#> 10  7523125878 7186840767
#> 11  7524769973 7186840767
#> 12  7528082928 7186840767
#> 13  7528082999 7186840767
#> 14  7528083090 7186840767
#> 15  7528087811 7186840767
#> 16  7844472994 7186840767
#> 17  7844473351 7186840767
#> 18  7866011642 7186840767
#> 19  7866015978 7186840767
#> 20  7869320680 7186840767
#> 21  8184049304 7186840767
#> 22  8184049546 7186840767
#> 23  8187380528 7186840767
#> 24  8193975632 7186840767
#> 25  8198954037 7186840767
#> 26  8515392737 7186840767
#> 27  8525318026 7186840767
#> 28  8525318518 7186840767
#> 29  8525318619 7186840767
#> 30  8525318881 7186840767
#> 31  8526948746 7186840767
#> 32  8535280040 7186840767
#> 33  8541870917 7186840767
#> 34  8543523319 7186840767
#> 35  8861569429 7186840767
#> 36  8866569630 7186840767
#> 37  8866569764 7186840767
#> 38  8869882415 7186840767
#> 39  8873165227 7186840767
#> 40  8873192134 7186840767
#> 41  8874821377 7186840767
#> 42  8876521709 7186840767
#> 43  8876543998 7186840767
#> 44  8878156810 7186840767
#> 45  8881456848 7186840767
#> 46  8883135153 7186840767
#> 47  9217782462 7186840767
#> 48  9219408058 7186840767
#> 49  9219408083 7186840767
#> 50  9221047258 7186840767
#> 51  9537450356 7186840767
#> 52  9539102248 7186840767
#> 53  9540758370 7186840767
#> 54  9540758400 7186840767
#> 55  9540758403 7186840767
#> 56  9554037005 7186840767
#> 57  9554041768 7186840767
#> 58  9555689874 7186840767
#> 59  9560632655 7186840767
#> 60  9562293635 7186840767
#> 61  9563945725 7186840767
#> 62  9875383416 7186840767
#> 63  9880358224 7186840767
#> 64  9885314546 7186840767
# }
```
