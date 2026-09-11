# Flytable database queries

`flytable_query` performs a SQL query against a flytable database. You
can omit the `base` argument unless you have tables of the same name in
different bases.

## Usage

``` r
flytable_list_rows(
  table,
  base = NULL,
  view_name = NULL,
  order_by = NULL,
  desc = FALSE,
  start = 0L,
  limit = Inf,
  collapse_lists = TRUE,
  python = FALSE,
  chunksize = NULL
)

flytable_query(
  sql,
  limit = 100000L,
  base = NULL,
  python = FALSE,
  convert = TRUE,
  collapse_lists = TRUE,
  paginate = TRUE,
  chunksize = NULL
)
```

## Arguments

- table:

  The name of a table inside your database

- base:

  Character vector naming a seatable base (recommended) or a `Base`
  object returned by `flytable_base` (expert use).

- view_name:

  An optional view which may limit the rows/columns displayed.

- order_by:

  Optional name of columns to order results

- desc:

  Whether to use descending order (default `FALSE` =\> ascending order)

- start:

  Optional starting row

- limit:

  An optional limit on the total number of rows returned, which only
  applies if you do not specify a limit directly in the `sql` query. By
  default seatable limits SQL queries to 100 rows. We increase the limit
  to 100000 rows by default. See `paginate` for how this interacts with
  the server's per-call row cap.

- collapse_lists:

  Whether to collapse any list multi-select columns into simple strings.
  The default value of `collapse_lists=TRUE` will comma separate them.

- python:

  Whether to return a Python pandas `DataFrame`. The default of `FALSE`
  returns an R `data.frame`

- chunksize:

  Optional maximum number of rows to request per web request. For
  advanced use only; the default `NULL` fetches as many rows per call as
  the server allows. For `flytable_query` a non-`NULL` value forces
  `LIMIT`/`OFFSET` pagination in windows of this size (mainly useful for
  exercising the paging path against a server whose own row cap is too
  high to reach with a modest table).

- sql:

  A SQL query string. See examples and [seatable
  docs](https://seatable.github.io/seatable-scripts/python/query/).

- convert:

  Expert use only: Whether or not to allow the Python seatable module to
  process raw output from the database. This is is principally for
  debugging purposes. NB this imposes a requirement of seatable_api
  \>=2.4.0.

- paginate:

  Whether to transparently page through large results with
  `LIMIT`/`OFFSET` (default `TRUE`). Seatable's SQL endpoint silently
  caps a single call at a server-specific maximum (documented default
  10,000 rows for SELECT queries,
  <https://api.seatable.com/reference/limits>; self-hosted servers may
  allow more) with no truncation warning, so without pagination a query
  matching more rows than the cap would silently lose rows. Pagination
  is skipped automatically when you supply your own `limit`/`offset` in
  the `sql`, when `python=TRUE`, or when the first page already returns
  fewer rows than the guaranteed cap.

## Value

An R `data.frame` or Pandas `DataFrame` depending on the value of the
`python` argument.

a `data.frame` of results. There should be 0 rows if no rows matched
query.

## Details

Flytable uses programmatic access to the
[seatable](https://seatable.github.io/seatable-scripts/) API.

## See also

[`tabify_coords`](https://natverse.org/fafbseg/reference/tabify_coords.md)
to help with copy-pasting coordinates to seatable.

Other flytable:
[`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md),
[`flytable_list_selected()`](https://natverse.org/fafbseg/reference/flytable_list_selected.md),
[`flytable_login()`](https://natverse.org/fafbseg/reference/flytable_login.md),
[`flytable_select_options()`](https://natverse.org/fafbseg/reference/flytable_update_rows.md)

## Examples

``` r
# \donttest{
flytable_list_rows(table = "testfruit")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named): requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3722a9b4c0>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
# }
# \donttest{
flytable_query("SELECT person, fruit_name FROM testfruit WHERE person!='Bob'")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714f3f100>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query("SELECT person, fruit_name FROM testfruit WHERE person!='Bob'"): I inferred table_name: testfruit from your SQL query but couldn't connect to a base with this table!
# }
if (FALSE) { # \dontrun{
flytable_query(paste("SELECT root_id, supervoxel_id FROM info limit 5"))
} # }
```
