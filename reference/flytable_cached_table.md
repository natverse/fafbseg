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
  expiry = 0,
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

  Seconds before checking for updates (default 0, i.e. always check for
  updates on every call). Set to a positive number of seconds to reduce
  network chatter by trusting the cache within that window, or `Inf` to
  never check i.e. to use what is available on disk. Note that an
  `expiry = 0` check is still cheap because it only downloads rows
  modified since the last sync (a delta sync).

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

  An optional limit on the total number of rows returned, which only
  applies if you do not specify a limit directly in the `sql` query. By
  default seatable limits SQL queries to 100 rows. We increase the limit
  to 100000 rows by default. See `paginate` for how this interacts with
  the server's per-call row cap.

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
[`flytable_select_options()`](https://natverse.org/fafbseg/reference/flytable_update_rows.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# First call - full fetch
info <- flytable_cached_table("info")

# Subsequent call - delta syncs any changes since the last fetch (default
# expiry = 0 always checks)
info2 <- flytable_cached_table("info")

# Trust the cache for 5 minutes before checking again
info3 <- flytable_cached_table("info", expiry = 300)

# Force complete re-download
info4 <- flytable_cached_table("info", refresh = TRUE)

# Check when data was last synced
attr(info, "mtime")
} # }
```
