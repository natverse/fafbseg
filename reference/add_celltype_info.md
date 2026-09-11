# Fetch flytable cell type information to a dataframe with flywire ids

`add_celltype_info` will add information to an existing dataframe.

`flytable_meta` will fetch a data.frame of metadata from flytable for a
given set of identifiers.

## Usage

``` r
add_celltype_info(
  x,
  idcol = NULL,
  version = NULL,
  suffix = NULL,
  table = c("both", "info", "optic"),
  ...
)

flytable_meta(
  ids = NULL,
  version = NULL,
  table = c("both", "info", "optic"),
  unique = FALSE,
  ...
)
```

## Arguments

- x:

  a data.frame containing root ids or a
  [`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) ()

- idcol:

  Optional character vector specifying the column containing ids of the
  neurons for which cell type information should be provided.

- version:

  Optional numeric CAVE version (see `flywire_cave_query`). The special
  signalling value of `TRUE` uses the current default data dump as
  returned by
  [`flywire_connectome_data_version`](https://natverse.org/fafbseg/reference/flywire_connectome_data.md).

- suffix:

  A character suffix for the new columns (default value of `NULL`
  implies no suffix).

- table:

  Which cell type information tables to use (`info` for brain, `optic`
  for optic lobes or `both`).

- ...:

  additional arguments passed to `flytable_cell_types`

- ids:

  Flywire identifiers/query in any form understood by
  [`flywire_ids`](https://natverse.org/fafbseg/reference/flywire_ids.md)

- unique:

  Whether to ensure that rows contain only unique identifiers. Default
  `FALSE`. When `TRUE` duplicate rows will be returned with a warning.

## Value

a data.frame with extra columns

## Details

the root ids must be in a column called one of
`"pre_id", "post_id", "root_id", "post_pt_root_id", "pre_pt_root_id"`.
If you do not have exactly one of these columns present then you must
specify your preferred column with the `idcol` argument.

## See also

[`flytable_cell_types`](https://natverse.org/fafbseg/reference/flytable_cell_types.md)

## Examples

``` r
# \donttest{
kcin=flywire_partner_summary("720575940626474889", partners = 'in',
  cleft.threshold = 50)
kcin
#> # A tibble: 103 × 3
#>    query              pre_id             weight
#>    <chr>              <chr>               <int>
#>  1 720575940626474889 720575940640891763     29
#>  2 720575940626474889 720575940613583001     28
#>  3 720575940626474889 720575940632698797     26
#>  4 720575940626474889 720575940615834258     25
#>  5 720575940626474889 720575940607687260     19
#>  6 720575940626474889 720575940642086389     14
#>  7 720575940626474889 720575940622673703      8
#>  8 720575940626474889 720575940620709681      6
#>  9 720575940626474889 720575940621178206      6
#> 10 720575940626474889 720575940619665023      5
#> # ℹ 93 more rows
kcin2=add_celltype_info(kcin)
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714f3da20>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
kcin2
#> Error: object 'kcin2' not found
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:nat’:
#> 
#>     intersect, setdiff, union
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
kcin2 %>%
  group_by(cell_type) %>%
  summarise(wt = sum(weight),n=n()) %>%
  arrange(desc(wt))
#> Error: object 'kcin2' not found
kcin2 %>%
  count(cell_class, wt = weight)
#> Error: object 'kcin2' not found
# }

if (FALSE) { # \dontrun{
# read neuronlist containing "dotprops" for some olfactory projection neurons
da2=read_l2dps('DA2')
# add cell type details to that
da2=add_celltype_info(da2)
} # }
# \donttest{
flytable_meta("class:MBON")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714f3ee30>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
flytable_meta("type:MBON2%")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714fc4310>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# the / introduces a regex query (small performance penalty, more flexible)
flytable_meta("/type:MBON2[0-5]")
#> Error in py_call_impl(callable, call_args$unnamed, call_args$named) : 
#>   requests.exceptions.ConnectTimeout: HTTPSConnectionPool(host='flytable.mrc-lmb.cam.ac.uk', port=443): Max retries exceeded with url: /api/v2.1/workspaces/ (Caused by ConnectTimeoutError(<HTTPSConnection(host='flytable.mrc-lmb.cam.ac.uk', port=443) at 0x7f3714fc57b0>, 'Connection to flytable.mrc-lmb.cam.ac.uk timed out. (connect timeout=30)'))
#> Run `reticulate::py_last_error()` for details.
#> Error in flytable_query(paste("select", fields, "FROM ", table, "WHERE status NOT IN (\"bad_nucleus\", \"duplicate\", \"not_a_neuron\")",     "AND", likeline)): I inferred table_name: info from your SQL query but couldn't connect to a base with this table!
# }
```
