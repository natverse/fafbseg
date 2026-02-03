# Read or return path to FlyWire annotations manuscript supplementary file

`flywire_sirepo_file_memo()` is a memoised version with a 5 minute
timeout

## Usage

``` r
flywire_sirepo_file(
  p,
  mustWork = NA,
  read = FALSE,
  version = c(783L, 630L),
  repo = "flyconnectome/flywire_annotations",
  ref = NULL,
  ...
)

flywire_sirepo_file_memo(
  p,
  mustWork = NA,
  read = FALSE,
  version = c(783L, 630L),
  repo = "flyconnectome/flywire_annotations",
  ref = NULL,
  ...
)
```

## Arguments

- p:

  Relative path to file within flywire_annotations repository *or* full
  URL to the file on github (nb in this case `repo` argument is
  ignored).

- mustWork:

  Whether the path must exists (default `NA` =\> `TRUE` when reading the
  file)

- read:

  Whether to read the file. Either a logical value or a function. When
  `TRUE` and `p` is a tsv or csv file a default read function is used
  (see details).

- version:

  An integer CAVE materialisation version (see
  [`flywire_connectome_data_version`](https://natverse.org/fafbseg/reference/flywire_connectome_data.md))

- repo:

  The github repository containing annotations (expert use only,
  defaults to the Schlegel et al flywire repo)

- ref:

  An optional github tag or branch (expert use only)

- ...:

  Additional arguments passed to the function determined by the `read`
  argument (typically
  `data.table::`[`fread`](https://rdrr.io/pkg/data.table/man/fread.html)).

## Value

A path or (when `read=TRUE` or a function) the result of reading the
file (a `data.table` for csv/tsv files).

## Details

When `read=TRUE` and `p` is a tsv or csv file them the
[`data.table::fread`](https://rdrr.io/pkg/data.table/man/fread.html)
function is used in order to ensure that 64 bit integers are correctly
parsed. The default behaviour is to read ids as character vectors but
this can be overridden (see examples).

`txt` files are read by `readLines` while `feather` files are read by
[`arrow::read_feather`](https://arrow.apache.org/docs/r/reference/read_feather.html)
when `read=TRUE`.

Since `flywire_sirepo_file` does an ~ 1 second check to see if the git
repository is up to date whenever you use it, you probably want to use
`flywire_sirepo_file_memo` in most cases.

## Examples

``` r
if (FALSE) { # \dontrun{
annpath=flywire_sirepo_file('supplemental_files/Supplemental_file1_annotations.tsv',
  read=FALSE)
# read in annotation file
anns=flywire_sirepo_file('supplemental_files/Supplemental_file1_annotations.tsv',
  read = TRUE)
# read in annotation file with ids as 64 bit integers rather than strings
anns=flywire_sirepo_file('supplemental_files/Supplemental_file1_annotations.tsv',
  read = TRUE, integer64="integer64")
# same but memoised to avoid checking github / re-reading file
anns=flywire_sirepo_file_memo('supplemental_files/Supplemental_file1_annotations.tsv',
  read = TRUE, integer64="integer64")
} # }
```
