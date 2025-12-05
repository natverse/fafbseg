# Fetch data specified by a set of curl shell commands (e.g. from Chrome)

Fetch data specified by a set of curl shell commands (e.g. from Chrome)

## Usage

``` r
fetch_all_curl(
  x = clipr::read_clip(),
  outdir = NULL,
  regex = "^curl",
  filename = "chunk%05d.raw",
  ...
)
```

## Arguments

- x:

  Path to a file or character vector of statements (by default it reads
  statements from the clipboard)

- outdir:

  Optional output directory (will be created if necessary)

- regex:

  Optional regular expression that curl statements must match

- filename:

  A [`sprintf`](https://rdrr.io/r/base/sprintf.html) style format
  statement that will be used to name the downloaded files.

- ...:

  Additional arguments to [`grep`](https://rdrr.io/r/base/grep.html)

## Value

A named character vector containing the matched URLs named by the
downloaded files on disk (invisibly)

## Details

You can generate an appropriate set of commands by opening the Chrome
Developer console (View ... Developer ... JavaScript Console),
(re)loading a page of interest, selecting the network tab, selecting a
downloaded object, right clicking and then choosing (Copy ... Copy all
as cURL). You should make sure that you only have one neuron displayed
if you do not want to have to parse the object identifier relationships.

## See also

[`read_ng_dump`](https://natverse.org/fafbseg/reference/read_ng_dump.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_all_curl("all_curl.sh", outdir="alldata",
  regex="brainmaps.googleapis.com", fixed=TRUE)
} # }
```
