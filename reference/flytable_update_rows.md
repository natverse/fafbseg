# Update or append rows in a flytable database

`flytable_update_rows` updates existing rows in a table, returning
`TRUE` on success.

`flytable_append_rows` appends data to an existing table, returning
`TRUE` on success.

`flytable_nrow` returns the number or rows in one or more flytable
tables using a SQL `COUNT` query.

## Usage

``` r
flytable_update_rows(
  df,
  table,
  base = NULL,
  append_allowed = TRUE,
  chunksize = 1000L,
  ...
)

flytable_append_rows(df, table, base = NULL, chunksize = 1000L, ...)

flytable_nrow(table, base = NULL)
```

## Arguments

- df:

  A data.frame containing the data to upload including an `_id` column
  that can identify each row in the remote table.

- table:

  Character vector naming a table

- base:

  Character vector naming a seatable base (recommended) or a `Base`
  object returned by `flytable_base` (expert use).

- append_allowed:

  Whether rows without row identifiers can be appended.

- chunksize:

  To split large requests into smaller ones with max this many rows.

- ...:

  Additional arguments passed to
  [`pbsapply`](https://peter.solymos.org/pbapply/reference/pbapply.html)
  which might include `cl=2` to specify a number of parallel jobs to
  run.

## Value

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

## See also

Other flytable:
[`flytable-queries`](https://natverse.org/fafbseg/reference/flytable-queries.md),
[`flytable_list_selected()`](https://natverse.org/fafbseg/reference/flytable_list_selected.md),
[`flytable_login()`](https://natverse.org/fafbseg/reference/flytable_login.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fruit=flytable_list_rows('testfruit')
flytable_update_rows(table='testfruit', fruit[1:2, c(1,4:6)])
} # }
if (FALSE) { # \dontrun{
flytable_append_rows(table="testfruit",
  data.frame(fruitname='lemon', person='David', nid=4))
} # }
```
