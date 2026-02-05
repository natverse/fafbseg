# Get a complete flytable table with disk caching and delta sync

Fetches a complete flytable table using disk caching with intelligent
delta synchronization. On first call, downloads the full table. On
subsequent calls within `expiry` seconds, returns cached data. After
expiry, performs delta sync fetching only rows modified since last sync
based on the `_mtime` column.

## Usage

``` r
flytable_cached_table(
  table,
  expiry = 300,
  refresh = FALSE,
  collapse_lists = TRUE,
  base = NULL,
  limit = 100000L
)
```

## Arguments

- table:

  Table name (e.g., "info", "optic", "testfruit")

- expiry:

  Seconds before checking for updates (default 300 = 5 minutes). Set to
  0 to always check for updates, `Inf` to never check.

- refresh:

  Logical. If `TRUE`, forces a complete re-download ignoring any cached
  data. Default `FALSE`.

- collapse_lists:

  Logical. If `TRUE` (default), collapses multi-select columns into
  comma-separated strings. Passed to
  [`flytable_query`](https://natverse.org/fafbseg/reference/flytable-queries.md).

- base:

  Optional base name if table name is ambiguous (exists in multiple
  bases).

- limit:

  An optional limit, which only applies if you do not specify a limit
  directly in the `sql` query. By default seatable limits SQL queries to
  100 rows. We increase the limit to 100000 rows by default.

## Value

A `data.frame` containing all rows from the table. Has an `mtime`
attribute recording the server timestamp at last sync.

## Details

The function uses the same disk cache infrastructure as
[`flywire_leaves`](https://natverse.org/fafbseg/reference/flywire_leaves.md).
The cache location can be controlled via the `fafbseg.cachedir` option.

Delta synchronization works by:

1.  Checking if cached data exists and is within the expiry window

2.  If expired, querying rows where `_mtime > cached_mtime`

3.  Updating modified rows, appending new rows

4.  Detecting and removing deleted rows via row count comparison

Error handling:

- Connection failures during sync: returns cached data with warning

- Schema changes (columns differ): forces full refresh

- Corrupted cache: clears and re-fetches

## See also

[`flytable_query`](https://natverse.org/fafbseg/reference/flytable-queries.md),
[`flytable_list_rows`](https://natverse.org/fafbseg/reference/flytable-queries.md)

Other flytable:
[`flytable-queries`](https://natverse.org/fafbseg/reference/flytable-queries.md),
[`flytable_list_selected()`](https://natverse.org/fafbseg/reference/flytable_list_selected.md),
[`flytable_login()`](https://natverse.org/fafbseg/reference/flytable_login.md),
[`flytable_update_rows()`](https://natverse.org/fafbseg/reference/flytable_update_rows.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# First call - full fetch
info <- flytable_cached_table("info")

# Subsequent call within 5 min - returns cached data
info2 <- flytable_cached_table("info")

# Force check for updates (ignores expiry window)
info3 <- flytable_cached_table("info", expiry = 0)

# Force complete re-download
info4 <- flytable_cached_table("info", refresh = TRUE)

# Check when data was last synced
attr(info, "mtime")
} # }
```
