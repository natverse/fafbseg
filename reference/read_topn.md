# Find and read the largest n segments from one or more skeleton zip files

`read_topn` calls `find_topn` to find the top fragments and then reads
them in using `read_segments2`.

`find_topn` finds the files, returning a `tibble` (`data.frame`) of
results.

## Usage

``` r
read_topn(zipfiles, n = 1, ...)

find_topn(zipfiles, n = 1, decreasing = TRUE)
```

## Arguments

- zipfiles:

  The path, name, or number of the zipfiles. If this is not a full path
  then it will be searched for in the location defined by
  options('fafbseg.skelziproot')

- n:

  Number of segments to read

- ...:

  additional arguments passed to
  [`read_segments2`](https://natverse.org/fafbseg/reference/read_segments.md)

- decreasing:

  Whether to sort the skeletons in decreasing order i.e. largest first
  (default=`TRUE`)

## Details

Note that this will read all the fragment skeletons for each segment

## See also

[`read_segments`](https://natverse.org/fafbseg/reference/read_segments.md)

## Examples

``` r
if (FALSE) { # \dontrun{
top3=read_topn("224270.zip", n=3)
} # }
```
