# Fast adjacency matrices based on flywire connectome dumps

Fast adjacency matrices based on flywire connectome dumps

## Usage

``` r
flywire_adjacency_matrix2(
  rootids = NULL,
  inputids = NULL,
  outputids = NULL,
  sparse = TRUE,
  threshold = 0,
  version = NULL,
  Verbose = interactive()
)
```

## Arguments

- rootids:

  flywire root ids for the bodies to fetch all by all connectivity
  information.

- inputids, outputids:

  identifiers for input and output bodies (use as an alternative to
  `rootids`)

- sparse:

  Whether to return a sparse matrix (default `FALSE`)

- threshold:

  For `flywire_partner_summary` only return partners with greater than
  this number of connections to the query neuron(s) (default of 0
  returns all connections)

- version:

  Optional CAVE version. The default value of `NULL` uses the latest
  data dump available unless
  `options(fafbseg.flywire_connectome_data_version)` has been set (which
  you can conveniently do using
  [`flywire_connectome_data_version()`](https://natverse.org/fafbseg/reference/flywire_connectome_data.md)).
  The special version of `"783.2"` will use the 2025 (Princeton) synapse
  data release.

- Verbose:

  Logical indication whether to print status messages during the query
  (default `T` when interactive, `F` otherwise).

## Value

A sparse matrix (`Matrix::dgCMatrix`) or regular `matrix`.

## Examples

``` r
if (FALSE) { # \dontrun{
dm2pnkc=flywire_adjacency_matrix2(inputids="DM2_lPN_R", outputids="class:Kenyon_Cell_R")
} # }
```
