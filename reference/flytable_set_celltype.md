# Set the flytable cell type and other columns for a set of root ids

Set the flytable cell type and other columns for a set of root ids

## Usage

``` r
flytable_set_celltype(
  ids,
  cell_type = NULL,
  hemibrain_type = NULL,
  user = "GJ",
  supervoxel_id = NULL,
  DryRun = TRUE,
  table = c("info", "optic"),
  copy_hemibrain_type = FALSE,
  ...
)
```

## Arguments

- ids:

  In any form understood by
  [`flywire_ids`](https://natverse.org/fafbseg/reference/flywire_ids.md).
  If this a data.frame and a `supervoxel_id` column is also present then
  that will be used to ensure that any root ids are efficiently mapped
  to their latest version.

- cell_type:

  Character vector of cell types - can be either of length 1 or of the
  length of ids.

- hemibrain_type:

  Will also be used to set cell_type if missing and
  `copy_hemibrain_type=TRUE`

- user:

  user initials (can be a vector of length ids)

- supervoxel_id:

  Supervoxel ids corresponding to the root `ids` argument.

- DryRun:

  Default value (T) will return the dataframe that would be used to
  update

- table:

  The name of a flytable table to update (currently we only support the
  info (central brain) and optic tables.

- copy_hemibrain_type:

  The recommendation is now \*not\* to set the cell_type from
  hemibrain_type if only one is provided, so default is `FALSE`.

- ...:

  Additional columns to update in flytable

## Details

By default `DryRun=TRUE` so that you can see what would happen if you
apply your changes. **Be careful!** Undoing changes is hard, sometimes
impossible exactly. If in doubt talk to Greg, Philipp et al before
making programmatic updates.

Since flytable always keeps root ids up to date (at least every 30m),
the `ids` argument must be mapped onto the latest ids (using
[`flywire_updateids`](https://natverse.org/fafbseg/reference/flywire_updateids.md))
before data can be uploaded to flytable. This will be much more
efficient if you provide a supervoxel id for each neuron.

## See also

[`flywire_updateids`](https://natverse.org/fafbseg/reference/flywire_updateids.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flytable_set_celltype('https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/4668606109450240',
hemibrain_type = 'vLN25', fbbt_id='FBbt_20003784', DryRun = T)
} # }
```
