# Fetch (memoised) flywire cell type information from flytable

Fetch (memoised) flywire cell type information from flytable

## Usage

``` r
flytable_cell_types(
  pattern = NULL,
  version = NULL,
  timestamp = NULL,
  target = c("type", "cell_type", "hemibrain_type", "cell_class", "super_class",
    "cell_sub_class", "ito_lee_hemilineage", "malecns_type", "all"),
  table = c("info", "optic", "both"),
  transfer_hemibrain_type = c("extra", "none", "all"),
  cache = TRUE,
  use_static = NA
)
```

## Arguments

- pattern:

  Optional character vector specifying a pattern that cell types must
  match in a SQL `LIKE` statement executed by
  [`flytable_query`](https://natverse.org/fafbseg/reference/flytable-queries.md).
  The suffix `_L` or `_R` can be used to restricted to neurons annotated
  to the L or R hemisphere. See examples.

- version:

  An optional CAVE materialisation version number. See
  [`flywire_cave_query`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)
  for more details. Note also that the special signalling value of
  `TRUE` implies the latest locally available connectome dump.

- timestamp:

  An optional timestamp as a string or POSIXct, interpreted as UTC when
  no timezone is specified.

- target:

  A character vector specifying which flytable columns `pattern` should
  match. The special value of `type` means either `cell_type` *or*
  `hemibrain_type` should match. The special value of `all` means to
  match against any of `cell_type, hemibrain_type, cell_class`.

- table:

  Which cell type information tables to use (`info` for brain, `optic`
  for optic lobes or `both`).

- transfer_hemibrain_type:

  Whether to transfer the `hemibrain_type` column into the `cell_type`
  (default TRUE, see details)

- cache:

  Whether to cache the results for 5m (default `TRUE` since the flytable
  query is is a little expensive)

- use_static:

  Whether to use static cell type information (from Schlegel et al)

## Value

The original data.frame left joined to appropriate rows from flytable.

## Details

when `transfer_hemibrain_type=TRUE`, `hemibrain_type` values will be
transferred into the `cell_type` column if `cell_type` is empty.

It seems that SQL LIKE searches (e.g. containing the `%` symbol) do not
work for the `ito_lee_hemilineage` column. You can still search for
exact matches or use full regular expression queries (which operate by
downloading all rows and then filtering on your machine).

Static cell type information is provided by Schlegel et 2023. See
[flywire_annotations](https://github.com/flyconnectome/flywire_annotations)
github repository. It will be used by default when connection to the
pre-release Cambridge flytable is not available or when specified by
`options(fafbseg.use_static_celltypes=TRUE)`. Note that presently only
one materialisation version (630) is supported for static data.

## See also

[`add_celltype_info`](https://natverse.org/fafbseg/reference/add_celltype_info.md)

## Examples

``` r
# \donttest{
flytable_cell_types("MBON%")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714fc6230>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
flytable_cell_types("MBON%", version=450)
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714fc7520>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# the latest connectome dump, see flywire_connectome_data_version()
if (FALSE) { # \dontrun{
flytable_cell_types("MBON%", version=TRUE)
} # }

# two characters
flytable_cell_types("MBON__")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714ff4850>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# at least one character
flytable_cell_types("MBON_%")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714ff5cf0>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# range
flytable_cell_types("MBON2[0-5]")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714ff7190>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!

# include side specification
flytable_cell_types("DA2_lPN_R")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f37224d0670>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# only the RHS MBON20
flytable_cell_types("MBON20_R")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f37224d1b10>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# all RHS cells with class MBON
flytable_cell_types("MBON_R", target="cell_class")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f37224d2fb0>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!

# anything with type *OR* class information
cells=flytable_cell_types(target = 'all')
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714494490>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# anything that mentions PN anywhere
pncands=flytable_cell_types('%PN%', target = 'all')
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714495930>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# }
```
