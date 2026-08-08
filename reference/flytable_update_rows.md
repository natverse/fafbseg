# Update or append rows in a flytable database

`flytable_select_options` returns the option names currently defined for
one or more single- or multiple-select columns.

`flytable_add_select_options` adds one or more new options to a single-
or multiple-select column's vocabulary.

`flytable_update_rows` updates existing rows in a table, returning
`TRUE` on success.

`flytable_append_rows` appends data to an existing table, returning
`TRUE` on success. You do not need a `_id` column as part of your input
for this to work.

`flytable_nrow` returns the number or rows in one or more flytable
tables using a SQL `COUNT` query.

## Usage

``` r
flytable_select_options(table, col = NULL, base = NULL)

flytable_add_select_options(table, col, options, base = NULL)

flytable_update_rows(
  df,
  table,
  base = NULL,
  append_allowed = TRUE,
  chunksize = 1000L,
  multi_select_cols = NULL,
  allow_new_options = FALSE,
  ...
)

flytable_append_rows(
  df,
  table,
  base = NULL,
  chunksize = 1000L,
  multi_select_cols = NULL,
  allow_new_options = FALSE,
  ...
)

flytable_nrow(table, base = NULL)
```

## Arguments

- table:

  Character vector naming a table

- col:

  Character vector of single- or multiple-select column name(s). The
  default `NULL` returns options for every such column in the table.

- base:

  Character vector naming a seatable base (recommended) or a `Base`
  object returned by `flytable_base` (expert use).

- options:

  Character vector of new option name(s) to add.

- df:

  A data.frame containing the data to upload including an `_id` column
  that can identify each row in the remote table.

- append_allowed:

  Whether rows without row identifiers can be appended.

- chunksize:

  To split large requests into smaller ones with max this many rows.

- multi_select_cols:

  Character vector of column names to treat as multiple-select
  (list-per-cell) columns. The default `NULL` auto-detects these from
  the table's column metadata.

- allow_new_options:

  When a multi-select value is not already a defined option for its
  column, whether to add it automatically (via
  `flytable_add_select_options`) rather than raising an error.

- ...:

  Additional arguments passed to
  [`pbsapply`](https://peter.solymos.org/pbapply/reference/pbapply.html)
  which might include `cl=2` to specify a number of parallel jobs to
  run.

## Value

`flytable_select_options` a named list of character vectors (one per
column) giving that column's currently defined option names.

`flytable_add_select_options` the response from the seatable API,
invisibly.

Logical indicating success, invisibly (failures will normally cause
premature termination with errors written to the console).

## Details

seatable automatically maintains a unique id for each row in a `_id`
column. This is returned by flytable_query and friends. If you modify
data and then want to update again, you need to keep the column
containing this row `_id`.

You do not need to provide this `_id` column when appending new rows.
Indeed you will get a warning when doing so.

The `chunksize` argument is required because it seems that there is a
maximum of 1000 rows per update action.

Multiple-select columns (e.g. `initials`, `annotator`) need special
handling: seatable expects a genuine list of option names per cell,
otherwise it silently creates a new bogus option out of whatever string
it was given. By default any column that seatable reports as type
`"multiple-select"` is auto-detected and routed through this
list-per-cell path; pass `multi_select_cols` explicitly to override
detection. A plain scalar value (e.g. `"AB"`, or a comma-joined
`"AB,CD"`) is accepted as shorthand and split on commas – symmetric with
how a multi-select cell reads back by default – or you can supply a
list-column directly (e.g. `I(list(c("AB","CD")))`), which is also how
you write a literal option name that itself contains a comma (a
list-column cell is taken verbatim, not split). Either way every
resulting value is checked against the column's existing option
vocabulary and rejected (by default) unless it is already a known option
– see `allow_new_options` and `flytable_add_select_options`.

## See also

Other flytable:
[`flytable-queries`](https://natverse.org/fafbseg/reference/flytable-queries.md),
[`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md),
[`flytable_list_selected()`](https://natverse.org/fafbseg/reference/flytable_list_selected.md),
[`flytable_login()`](https://natverse.org/fafbseg/reference/flytable_login.md)

## Examples

``` r
# \donttest{
flytable_select_options("testfruit", "initials")
#> $initials
#>  [1] "AB"                                         
#>  [2] "CD"                                         
#>  [3] "EF"                                         
#>  [4] "zztest-allow-new-options"                   
#>  [5] "zztest-allow-new-options-20260806090909.154"
#>  [6] "zztest-allow-new-options-20260806091008.674"
#>  [7] "zztest-allow-new-options-20260806091133.306"
#>  [8] "zztest-allow-new-options-20260806091256.771"
#>  [9] "zztest-allow-new-options-20260806234705.956"
#> [10] "zztest-allow-new-options-20260806231622.141"
#> [11] "zztest-allow-new-options-20260806231952.238"
#> [12] "zztest-allow-new-options-20260806232219.920"
#> [13] "zztest-allow-new-options-20260807033343.576"
#> [14] "zztest-allow-new-options-20260807033546.735"
#> [15] "zztest-allow-new-options-20260807033645.979"
#> [16] "zztest-allow-new-options-20260807050451.091"
#> [17] "zztest-allow-new-options-20260807050706.787"
#> [18] "zztest-allow-new-options-20260807050942.710"
#> [19] "zztest-allow-new-options-20260807113708.958"
#> [20] "zztest-allow-new-options-20260807113935.288"
#> [21] "zztest-allow-new-options-20260807114015.189"
#> [22] "zztest-allow-new-options-20260807115513.688"
#> [23] "zztest-allow-new-options-20260807115614.215"
#> [24] "zztest-allow-new-options-20260807115908.771"
#> [25] "zztest-allow-new-options-20260807121608.172"
#> [26] "zztest-allow-new-options-20260807121611.270"
#> [27] "zztest-allow-new-options-20260807140539.235"
#> [28] "zztest-allow-new-options-20260807140803.047"
#> [29] "zztest-allow-new-options-20260807140857.255"
#> [30] "zztest-allow-new-options-20260807144305.796"
#> [31] "zztest-allow-new-options-20260807212759.498"
#> [32] "zztest-allow-new-options-20260807225902.752"
#> [33] "zztest-allow-new-options-20260807230734.014"
#> [34] "zztest-allow-new-options-20260807225231.885"
#> [35] "zztest-allow-new-options-20260807225419.575"
#> [36] "zztest-allow-new-options-20260807232417.812"
#> [37] "zztest-allow-new-options-20260807232455.095"
#> [38] "zztest-allow-new-options-20260807232548.869"
#> [39] "zztest-allow-new-options-20260807232708.903"
#> [40] "zztest-allow-new-options-20260807232725.355"
#> [41] "zztest-allow-new-options-20260808065126.205"
#> [42] "zztest-allow-new-options-20260808065205.990"
#> [43] "zztest-allow-new-options-20260808065257.394"
#> [44] "zztest-allow-new-options-20260808103814.458"
#> [45] "zztest-allow-new-options-20260808104049.749"
#> [46] "zztest-allow-new-options-20260808094324.660"
#> [47] "zztest-allow-new-options-20260808094730.510"
#> [48] "zztest-allow-new-options-20260808094739.780"
#> [49] "zztest-allow-new-options-20260808095218.329"
#> [50] "zztest-allow-new-options-20260808095305.148"
#> [51] "zztest-allow-new-options-20260808095413.678"
#> [52] "zztest-allow-new-options-20260808101748.882"
#> [53] "zztest-allow-new-options-20260808102030.663"
#> [54] "zztest-allow-new-options-20260808102204.856"
#> [55] "zztest-allow-new-options-20260808112719.490"
#> [56] "zztest-allow-new-options-20260808112747.510"
#> [57] "zztest-allow-new-options-20260808112810.012"
#> [58] "zztest-allow-new-options-20260808112903.209"
#> [59] "zztest-allow-new-options-20260808112928.692"
#> [60] "zztest-allow-new-options-20260808113020.653"
#> 
# }
if (FALSE) { # \dontrun{
flytable_add_select_options("testfruit", "initials", "AN")
} # }
if (FALSE) { # \dontrun{
fruit=flytable_list_rows('testfruit')
flytable_update_rows(table='testfruit', fruit[1:2, c(1,4:6)])

# writing a multiple-select column
flytable_update_rows(table='testfruit',
  data.frame(row_id=fruit$`_id`[1], initials=I(list(c("AB","CD")))))
} # }
if (FALSE) { # \dontrun{
flytable_append_rows(table="testfruit",
  data.frame(fruitname='lemon', person='David', nid=4))
} # }
```
