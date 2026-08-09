# Return neuron metadata from Cambridge seatables

This function is a generic building block for access to experimental/in
progress neuron metadata. It is intended for internal use and the end
user or developer is responsible for choosing the active CAVE dataset
(see
[`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)).

## Usage

``` r
cam_meta(
  ids = NULL,
  ignore.case = F,
  fixed = F,
  table = "aedes_main",
  base = NULL,
  version = NULL,
  timestamp = NULL,
  unique = FALSE,
  translate_ids = NA,
  token = NULL,
  drop_status = c("duplicate", "bad_nucleus"),
  ...
)
```

## Arguments

- ids:

  Root ids (as character or int64 vector) or a query (see examples)

- ignore.case:

  for queries whether to ignore the case

- fixed:

  whether to treat queries as a fixed string

- table:

  The name of the table to query

- base:

  Optional name of the seatable base containing the table (sometimes the
  table may not be found or two bases contain a table with the same
  name).

- version:

  Integer materialisation version. The special value of `'latest'` means
  the most recent materialisation according to CAVE.

- timestamp:

  A timestamp to normalise into an R or Python timestamp in UTC. The
  special value of `'now'` means the current time in UTC.

- unique:

  Whether to drop rows that have the same root_id. See details. There is
  no special logic in choosing which rows to drop, but the dropped rows
  are retained as an attribute on the table with a warning so that you
  can inspect.

- translate_ids:

  Whether to bring explicitly supplied `ids` forward to the requested
  `version`/`timestamp` before matching. `NA` (the default) turns this
  on automatically when a `version` or `timestamp` is given (see
  details).

- token:

  Optional API token. When supplied, the `FLYTABLE_TOKEN` environment
  variable is temporarily set to this value for the duration of the call
  (and restored on exit) so you can authenticate against an alternative
  seatable instance without permanently overwriting your token.
  Typically used in combination with the `fafbseg.flytable.url` [package
  option](https://natverse.org/fafbseg/reference/fafbseg-package.md).

- drop_status:

  Character vector of `status` tokens whose rows are dropped before any
  query/join/`unique` step. Matching is case-insensitive and token-wise,
  so it also handles multi-select `status` columns holding
  comma-separated tokens (e.g. CRANT's capitalised `DUPLICATED`). Pass
  `NULL` or `character(0)` to keep every row.

- ...:

  Additional arguments passed to
  [`flytable_cached_table`](https://natverse.org/fafbseg/reference/flytable_cached_table.md)
  (e.g. `expiry`, `refresh`) which can be used to control details of the
  cache strategy.

## Value

A data frame with appropriate rows based on the `ids` argument.

## Details

This function now uses
[`flytable_cached_table`](https://natverse.org/fafbseg/reference/flytable_cached_table.md)
for efficient row-wise caching of metadata. The defaults should be a
good trade-off between cache speed and getting the latest updates, but
you can set `expiry = 0` if you want to ensure that you are as up to
date as possible - this still only downloads new changes and is very
fast (300ms vs 100ms for a pre-cached dataset with 14k rows).

Note that rows whose \`status\` matches \`drop_status\` (by default
\`duplicate\` or \`bad_nucleus\`) are dropped even before the \`unique\`
argument is processed. Matching is case-insensitive and token-wise, so
it works for multi-select \`status\` columns holding comma-separated
tokens.

When `version` or `timestamp` are specified the table's root ids are
brought to that timepoint via the `supervoxel_id` column. For a query
string the match then happens against that mapped table, so no further
work is needed. For explicit root `ids` the join is by `root_id`, so ids
that are stale relative to the requested timepoint would silently fail
to match. `translate_ids` guards against this by bringing only the
unmatched ids forward with
[`flywire_latestid`](https://natverse.org/fafbseg/reference/flywire_latestid.md)
(ids already present in the mapped table need no work, and
`flywire_latestid` only does a supervoxel lookup for genuinely outdated
ids). The default (`NA`) enables it whenever a `version`/`timestamp` is
supplied; with neither, nothing is translated since the table is simply
at the state of its last update.

If `translate_ids` is forced `TRUE` with no `version`/`timestamp`, ids
are aligned to the table's own sync time (its `mtime` attribute) rather
than live 'now', so both sides share a clock.

## Examples

``` r
# implies type
if (FALSE) { # \dontrun{
cam_meta("MBON.+")
cam_meta("class:ALPN")
# ensure that root ids match the most recent materialisation
cam_meta("class:ALPN", version='latest')

with_aedes(cam_meta)

} # }
```
