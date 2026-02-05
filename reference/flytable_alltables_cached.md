# Get all flytable tables with disk caching

Disk-cached version of
[`flytable_alltables`](https://natverse.org/fafbseg/reference/flytable_login.md)
that validates cache freshness using base metadata from
`list_workspaces()`. This dramatically speeds up repeated R sessions by
avoiding N API calls when the table structure hasn't changed.

## Usage

``` r
flytable_alltables_cached(refresh = FALSE)
```

## Arguments

- refresh:

  Logical. If `TRUE`, forces a complete refresh ignoring cache. Default
  `FALSE`.

## Value

A `data.frame` with columns `base_name`, `workspace_id`, `name` (table
name), and `_id`.

## Details

Invalidation strategy:

1.  Call `list_workspaces()` (single fast API call)

2.  Compare base count and max `updated_at` with cached metadata

3.  If unchanged, return disk-cached table list

4.  If changed, perform full refresh via
    [`flytable_alltables()`](https://natverse.org/fafbseg/reference/flytable_login.md)

## See also

[`flytable_alltables`](https://natverse.org/fafbseg/reference/flytable_login.md)

Other flytable:
[`flytable-queries`](https://natverse.org/fafbseg/reference/flytable-queries.md),
[`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md),
[`flytable_list_selected()`](https://natverse.org/fafbseg/reference/flytable_list_selected.md),
[`flytable_login()`](https://natverse.org/fafbseg/reference/flytable_login.md),
[`flytable_update_rows()`](https://natverse.org/fafbseg/reference/flytable_update_rows.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Fast startup - uses disk cache if structure unchanged
tables <- flytable_alltables_cached()

# Force refresh
tables <- flytable_alltables_cached(refresh = TRUE)
} # }
```
