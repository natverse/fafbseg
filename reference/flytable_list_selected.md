# List selected rows from flytable

List selected rows from flytable

## Usage

``` r
flytable_list_selected(
  ids = NULL,
  table = "info",
  fields = "*",
  idfield = "root_id",
  ...
)
```

## Arguments

- ids:

  One or more identifiers

- table:

  The name of the flytable table

- fields:

  The database columns to return

- idfield:

  Which field to use as a key for lookup

- ...:

  Additional arguments passed to
  [`flytable_query`](https://natverse.org/fafbseg/reference/flytable-queries.md)

## Value

a dataframe containing the selected rows / columns

## See also

Other flytable:
[`flytable-queries`](https://natverse.org/fafbseg/reference/flytable-queries.md),
[`flytable_alltables_cached()`](https://natverse.org/fafbseg/reference/flytable_alltables_cached.md),
[`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md),
[`flytable_login()`](https://natverse.org/fafbseg/reference/flytable_login.md),
[`flytable_update_rows()`](https://natverse.org/fafbseg/reference/flytable_update_rows.md)
