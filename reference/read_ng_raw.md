# Read raw neuroglancer data - currently only supports mesh format

Read raw neuroglancer data - currently only supports mesh format

## Usage

``` r
read_ng_raw(x, read_data = TRUE, Verbose = FALSE)
```

## Arguments

- x:

  Path to one or more files OR a directory (in which case all files are
  read).

- read_data:

  Whether to read the data (default when `TRUE`) or just the header

- Verbose:

  Whether to print some status messages (default `FALSE`)

## Value

An object of class 'ng_raw_list' containing one or more chunks of data
of class 'ng_raw'. When `x` contains multiple files, all the chunks are
merged into a single list.

## References

See <https://github.com/google/neuroglancer>

## Examples

``` r
if (FALSE) { # \dontrun{
res <- read_ng_raw("meshdata/chunk00789.raw")
resh <- read_ng_raw("meshdata/chunk00789.raw", read_data=FALSE)
resl <- read_ng_raw(dir("meshdata", full.names = TRUE))
} # }
```
