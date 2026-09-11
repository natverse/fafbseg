# Flexible specification of flywire ids (including from flytable types)

allows more flexible specification of flywire root ids compared with
[`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md)
including by queries against cell types recorded in flytable. Also
useful for reading ids from the clipboard or a file, which often consist
of a single whitespace or comma-delimited string.

## Usage

``` r
flywire_ids(
  x,
  file = NULL,
  integer64 = FALSE,
  check_latest = FALSE,
  must_work = FALSE,
  na_ok = FALSE,
  na.rm = FALSE,
  unique = FALSE,
  version = NULL,
  table = c("both", "info", "optic"),
  ...
)
```

## Arguments

- x:

  A character or bit64::integer64 vector or a dataframe specifying ids
  directly *or* a string specifying a query, a URL *or* a comma/space
  delimited list of ids (see examples).

- file:

  As an alternative to `x` the path to a file containing ids.

- integer64:

  Whether to return ids as 64 bit integers - more compact than character
  vector, but can be more fragile (default `FALSE`).

- check_latest:

  Whether to check if ids are up to date.

- must_work:

  Whether ids must be valid

- na_ok:

  whether NA ids are acceptable when `must_work=TRUE`

- na.rm:

  Whether to drop missing (`NA`) input ids. Applied before ids are
  coerced, so missing values never surface as the null segment `"0"`;
  genuine `"0"`/zero inputs are kept.

- unique:

  Whether to return only unique ids

- version:

  Integer materialisation version. The special value of `'latest'` means
  the most recent materialisation according to CAVE.

- table:

  When `x` is a query whether to search `brain`, `optic` lobe or `both`
  info tables.

- ...:

  Additional arguments passed to
  [`flytable_cell_types`](https://natverse.org/fafbseg/reference/flytable_cell_types.md)
  or
  [`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md).

## Value

character (or `integer64`)) vector of segment ids

## See also

[`flytable_cell_types`](https://natverse.org/fafbseg/reference/flytable_cell_types.md).

Other neuroglancer-urls:
[`flywire_scene()`](https://natverse.org/fafbseg/reference/flywire_scene.md),
[`ngl_blank_scene()`](https://natverse.org/fafbseg/reference/ngl_blank_scene.md),
[`ngl_decode_scene()`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md),
[`ngl_encode_url()`](https://natverse.org/fafbseg/reference/ngl_encode_url.md),
[`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md),
[`open_fafb_ngl()`](https://natverse.org/fafbseg/reference/open_fafb_ngl.md)

## Examples

``` r
flywire_ids(data.frame(root_id=1))
#> [1] "1"
flywire_ids(data.frame(root_id=1), integer64=TRUE)
#> integer64
#> [1] 1
# Bad values will return 0
flywire_ids(data.frame(root_id=-1))
#> [1] "0"
if (FALSE) { # \dontrun{
# will error
flywire_ids(data.frame(root_id=-1), must_work = TRUE)
} # }
# DL1 olfactory PNs
flywire_ids("DL1_adPN")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f37040b4ee0>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# DL1 olfactory PNs but only on the RHS
flywire_ids("DL1_adPN_R")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f37040b6080>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# specifying materialisation version
flywire_ids("DL1_adPN_R", version=630)
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f37040b7a30>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# using SQL wild cards
flywire_ids("DA[12]_%PN_L")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3704138eb0>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!

# all sensory neurons
flywire_ids("super:sensory", integer64=TRUE)
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f370413a800>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!

# note that side is defined by soma position (not arbour side)
flywire_ids("class:MBON_R", integer64=TRUE)
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f370413bf40>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# superclass can also have a side specified
flywire_ids("super:motor_R", integer64=TRUE)
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3696bcd120>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!

# you can also use a comma/whitespace delimited list
flywire_ids("1234, 123456")
#> [1] "1234"   "123456"
# ... which could come from the clipboard
if (FALSE) { # \dontrun{
flywire_ids(clipr::read_clip())

# ... or from a file
flywire_ids(file='~/Downloads/root_ids_Li02_.txt')
} # }
```
