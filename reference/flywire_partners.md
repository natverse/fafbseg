# Find the input/output partners for a flywire neuron

`flywire_partners` is a low level function returning one row for every
synapse.

`flywire_partner_summary` summarises the connectivity of one or more
flywire neurons.

## Usage

``` r
flywire_partners(
  rootids,
  partners = c("outputs", "inputs", "both"),
  details = FALSE,
  roots = TRUE,
  reference = c("either", "FAFB14", "FlyWire"),
  cloudvolume.url = NULL,
  method = c("auto", "spine", "sqlite"),
  Verbose = TRUE,
  local = NULL,
  ...
)

flywire_partner_summary(
  rootids,
  partners = c("outputs", "inputs"),
  threshold = 0,
  remove_autapses = TRUE,
  cleft.threshold = 0,
  summarise = FALSE,
  surf = NULL,
  version = NULL,
  timestamp = NULL,
  chunksize = NULL,
  method = c("auto", "spine", "sqlite", "cave"),
  Verbose = NA,
  local = NULL,
  ...
)
```

## Arguments

- rootids:

  Character vector specifying one or more flywire rootids. As a
  convenience for `flywire_partner_summary` this argument is passed to
  [`flywire_ids`](https://natverse.org/fafbseg/reference/flywire_ids.md)
  allowing you to pass in data.frames, flywire URLs or cell type
  queries.

- partners:

  Whether to fetch input or output synapses or both.

- details:

  Whether to include additional details such as X Y Z location (default
  `FALSE`)

- roots:

  Whether to fetch the flywire rootids of the partner neurons (default
  `TRUE`)

- reference:

  A character vector or a `templatebrain` object specifying the
  reference template brain for any 3D coordinate information. The
  default value of `"either"` will use the natural reference space of
  the data source (FAFB14 for SQLite tables, FlyWire for the spine
  service).

- cloudvolume.url:

  The segmentation source URL for cloudvolume. Normally you can ignore
  this and rely on the default segmentation chosen by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)

- method:

  Whether to use a local SQLite database or remote spine service for
  synapse data. The default `auto` uses a local database when available
  (45GB but faster).

- Verbose:

  Whether to print status messages

- local:

  path to SQLite synapse data. Evaluated by `fafbseg:::local_or_google`.
  Work in progress. Default is to download this data and place it in
  `~/projects/JanFunke`.

- ...:

  Additional arguments passed to `pbsapply`

- threshold:

  For `flywire_partner_summary` only return partners with greater than
  this number of connections to the query neuron(s) (default of 0
  returns all connections)

- remove_autapses:

  For `flywire_partner_summary` whether to remove autapses (defaults to
  TRUE)

- cleft.threshold:

  A threshold for the cleft score calculated by Buhmann et al 2019
  (default 0, we have used 30-100 to increase specificity)

- summarise:

  (This was never implemented.) Whether to collapse down the results for
  multiple query neurons into a single entry for each partner neuron.

- surf:

  An object defining a 3D ROI inside which the presynaptic position must
  be located. Can be a `mesh3d` object, or any object which
  [`as.mesh3d`](https://rdrr.io/pkg/nat/man/as.mesh3d.html) can handle
  including [`hxsurf`](https://rdrr.io/pkg/nat/man/read.hxsurf.html) and
  [`boundingbox`](https://rdrr.io/pkg/nat/man/boundingbox.html) objects.
  See [`pointsinside`](https://rdrr.io/pkg/nat/man/pointsinside.html)
  for details.

- version:

  Integer materialisation version. The special value of `'latest'` means
  the most recent materialisation according to CAVE.

- timestamp:

  A timestamp to normalise into an R or Python timestamp in UTC. The
  special value of `'now'` means the current time in UTC.

- chunksize:

  (expert use) number of query neurons to send per chunk to cave client.
  Chunking requests speeds up queries (over sending neurons one a time)
  while still avoiding row number limits on queries with many neurons.

## Value

A `data.frame` with a `regtemplate` attribute specifying the reference
brain space for any xyz points. Columns vary slightly depending on
whether data is fetched from spine/ITANNA or a local sqlite database.
The more obscure ones include:

- `prepost` When `partners = "both"` then this column will be present.
  For any given row (synapse), `prepost=1` when the initial query neuron
  is downstream (postsynaptic) and the partner is upstream.

- `score` came straight from Buhmann et al and is supposed to indicate
  some confidence score for the predicted synapse.

- `cleft_score` is the more useful one and was a later addition by
  Stephan Gerhard that asks whether the pre and post synapses are
  positioned at sensible distances on each side of a cleft defined by a
  separate neural network from Larissa Heinrich in Stephan Saalfeld’s
  group.

## Details

FIXME behaviour when there are no partners is not well-defined.

Note that there are many duplicated connections in this raw output,
false autapses (i.e. the same neuron connected to itself) and other
false positives. See Buhmann et al for details and ideas about cleaning
up the results.

Also note that the ids returned are of the class `integer64` for
`flywire_partners` where there is one row for each synapses/connection;
but for `flywire_partner_summary` where rows report the number of
connections between pairs of neurons, they are of type `character`. This
is because the `integer64` type is more compact but less robust because
it is not a base R type but instead provided by the `bit64` package.
Some R functions such as `sapply` strip the class from `integer64`
vectors, treating them as doubles of a completely different value.

`flywire_partners` and `flywire_partner_summary` by default report on
the active connectivity state of neurons. At present only
`flywire_partner_summary` allows time travel to historic
materialisations using the `version` or `timestamp` arguments (see
[`flywire_timestamp`](https://natverse.org/fafbseg/reference/flywire_timestamp.md)
for details). This support actually depends on the cave backend (which
will automatically be selected when `method='auto'`).

## See also

Other automatic-synapses:
[`flywire_adjacency_matrix()`](https://natverse.org/fafbseg/reference/flywire_adjacency_matrix.md),
[`flywire_neurons_add_synapses()`](https://natverse.org/fafbseg/reference/flywire_neurons_add_synapses.md),
[`flywire_ntplot()`](https://natverse.org/fafbseg/reference/flywire_ntplot.md),
[`flywire_ntpred()`](https://natverse.org/fafbseg/reference/flywire_ntpred.md)

Other automatic-synapses:
[`flywire_adjacency_matrix()`](https://natverse.org/fafbseg/reference/flywire_adjacency_matrix.md),
[`flywire_neurons_add_synapses()`](https://natverse.org/fafbseg/reference/flywire_neurons_add_synapses.md),
[`flywire_ntplot()`](https://natverse.org/fafbseg/reference/flywire_ntplot.md),
[`flywire_ntpred()`](https://natverse.org/fafbseg/reference/flywire_ntpred.md)

## Examples

``` r
# \donttest{
pp=flywire_partners("720575940621039145")
#> Warning: /home/runner/projects/JanFunke//flywire_synapses.db does not exist
#> Fetching supervoxel ids for id: 720575940621039145
#> Error in `==.integer64`(as.integer64(strmax), maxint64): non-numeric argument to binary operator
head(pp)
#> Error: object 'pp' not found
class(pp$post_id)
#> Error: object 'pp' not found
# }
# \donttest{
# Note that post_id is of type character
flywire_partner_summary("720575940621039145", partners='out')
#> Error in py_call_impl(x, dots$unnamed, dots$named): ValueError: Timestamp incompatible with IDs: [720575940621039145] are expired,  use chunkedgraph client to find valid ID(s)
#> Run `reticulate::py_last_error()` for details.
flywire_partner_summary("720575940621039145", partners='in')
#> Error in py_call_impl(x, dots$unnamed, dots$named): ValueError: Timestamp incompatible with IDs: [720575940621039145] are expired,  use chunkedgraph client to find valid ID(s)
#> Run `reticulate::py_last_error()` for details.
flywire_partner_summary("720575940621039145")
#> Error in py_call_impl(x, dots$unnamed, dots$named): ValueError: Timestamp incompatible with IDs: [720575940621039145] are expired,  use chunkedgraph client to find valid ID(s)
#> Run `reticulate::py_last_error()` for details.

# summary for neuron at a XYZ location (in this case in raw coordinates)
flywire_partner_summary(flywire_xyz2id(cbind(155682, 58180, 3215),
  rawcoords = TRUE))
#> Error in `==.integer64`(as.integer64(strmax), maxint64): non-numeric argument to binary operator

if (FALSE) { # \dontrun{
# Use Ctrl+Shift+J to share a flywire scene and then do this to get partner
# summary for that URL
flywire_partner_summary(clipr::read_clip())

cct=flywire_cave_query('cambridge_celltypes', live = T)
dl1.lh=flywire_partner_summary(cct$pt_root_id[grep("DL1", cct$cell_type)],
  surf=subset(elmr::FAFB14NP.surf, "LH_R"))
  # use a rectangular bounding box around LH instead
dl1.lhbb=flywire_partner_summary(cct$pt_root_id[grep("DL1", cct$cell_type)],
  surf=boundingbox(subset(elmr::FAFB14NP.surf, "LH_R")))
} # }
# }
```
