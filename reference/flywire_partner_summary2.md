# Rapid flywire connectivity summaries using cached connectome data

Rapid flywire connectivity summaries using cached connectome data

## Usage

``` r
flywire_partner_summary2(
  ids,
  partners = c("outputs", "inputs"),
  add_cell_types = TRUE,
  by.roi = FALSE,
  summarise = FALSE,
  threshold = 0,
  version = NULL
)
```

## Arguments

- ids:

  Root ids to query (passed to
  [`flywire_ids`](https://natverse.org/fafbseg/reference/flywire_ids.md))

- partners:

  Whether to fetch input or output synapses or both.

- add_cell_types:

  Whether to add cell type information to the result

- by.roi:

  Whether to break the connectivity down into rows for each neuropil
  region containing synapses.

- summarise:

  Whether to collapse down the results for multiple query neurons into a
  single entry for each partner neuron.

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

## Value

A data.frame

## Details

Note that the threshold is applied to each row left after any grouping
operations. Therefore when `by.roi=TRUE` only neuropil regions exceeding
this threshold will be returned.

CAVE specifies versions (effectively timestamps) for the connectome
data. Every so often Sven makes a dump of the connectivity and synapse
information for all proofread neurons. At this point, the only versions
in use are 783 (released with the published articles in Nature) and 630
(released with the preprint).

The 783 release is available with both the original connectivity based
on Buhmann et at 2021 (still the default) as well as new v2 synapses
released in 2025 (version='783.2')

## Examples

``` r
if (FALSE) { # \dontrun{
flywire_partner_summary2('DA2_lPN', partners='out')
flywire_partner_summary2('DA2_lPN', partners='out', summarise=T)
flywire_partner_summary2('DA2_lPN', partners='out', summarise=T, by.roi=T)

flywire_partner_summary2('DA2_lPN', partners='out', summarise=T,
  by.roi=T, add_cell_types=F) %>%
  filter(!grepl("AL", neuropil)) %>%
  group_by(post_pt_root_id) %>%
  summarise(weight = sum(weight), top_np = neuropil[1]) %>%
  arrange(desc(weight)) %>%
  # nb version = TRUE will use ensure that ids match the default CAVE version
  add_celltype_info(version=TRUE)
} # }
```
