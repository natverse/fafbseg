# Attach synapses to flywire neuron skeletons

Attach the appropriate input and output synapses to each flywire neuron
skeleton in a neuronlist.

## Usage

``` r
flywire_neurons_add_synapses(
  x,
  connectors = NULL,
  cloudvolume.url = NULL,
  method = c("auto", "spine", "sqlite"),
  remove_autapses = TRUE,
  cleft.threshold = 0,
  Verbose = TRUE,
  transmitters = FALSE,
  local = NULL,
  ...
)

# S3 method for class 'neuron'
flywire_neurons_add_synapses(
  x,
  connectors = NULL,
  cloudvolume.url = NULL,
  method = c("auto", "spine", "sqlite"),
  remove_autapses = TRUE,
  cleft.threshold = 0,
  Verbose = TRUE,
  transmitters = FALSE,
  local = NULL,
  ...
)

# S3 method for class 'neuronlist'
flywire_neurons_add_synapses(
  x,
  connectors = NULL,
  cloudvolume.url = NULL,
  method = c("auto", "spine", "sqlite"),
  remove_autapses = TRUE,
  cleft.threshold = 0,
  Verbose = TRUE,
  transmitters = FALSE,
  local = NULL,
  ...
)

flywire_synapse_annotations(
  x,
  file = NULL,
  scale = 1/c(4, 4, 40),
  sample = NULL,
  best = TRUE,
  cleft.threshold = 30,
  remove_autapses = TRUE,
  local = NULL,
  cloudvolume.url = NULL
)
```

## Arguments

- x:

  a [`nat::neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) for
  flywire neurons in the FlyWire or FAFB14 brainspace. These skeletons
  can be created using
  [`skeletor`](https://natverse.org/fafbseg/reference/skeletor.md), or
  retrieved using `hemibrainr::flywire_neurons`. When using
  `flywire_synapse_annotations` this can be a `data.frame` of synapses,
  e.g. from `flywire_ntpred` that need to be formatted as FlyWire
  annotations.

- connectors:

  a `data.frame` of FAFB synapses, with XYZ coordinates, to attach to
  `x`. If `NULL` (default) synapses are fetched, as in
  [`flywire_partners`](https://natverse.org/fafbseg/reference/flywire_partners.md).

- cloudvolume.url:

  The segmentation source URL for cloudvolume. Normally you can ignore
  this and rely on the default segmentation chosen by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)

- method:

  Whether to use a local SQLite database or remote spine service for
  synapse data. The default `auto` uses a local database when available
  (45GB but faster).

- remove_autapses:

  whether to remove autapses (defaults to `TRUE`).

- cleft.threshold:

  select only synaptic connections exceeding this confidence threshold
  (default of 0 uses all synapses; values in the range 30-100 seem to
  make sense).

- Verbose:

  Whether to print status messages

- transmitters:

  if `TRUE` also attempt to retrieve neurotransmitter predictions from
  Eckstein and Bates et al. 2024, for the flywire neuron in question.

- local:

  path to SQLite synapse data. Evaluated by `fafbseg:::local_or_google`.
  Work in progress. Default is to download this data and place it in
  `~/projects/JanFunke`.

- ...:

  methods sent to
  [`nat::nlapply`](https://rdrr.io/pkg/nat/man/nlapply.html).

- file:

  when using `flywire_synapse_annotations`, the file path to which to
  output a `.csv`. If `NULL`, a `data.frame` formatted like a
  annotations CSV for FlyWire, is returned.

- scale:

  a scale factor applied to the XYZ coordinates for synapses. Default
  moves them from nanometre FlyWire space to raw voxel FlyWire space,
  which is most appropriate for FlyWire annotations.

- sample:

  if an integer, this is the number of synapses that are sampled from
  `x`.

- best:

  logical. If `TRUE` and sample is an integer, then the synapses with
  the highest cleft scores are chosen, `1:sample`.

## Value

A [`nat::neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html)
object, where each neuron in the neuronlist has a `data.frame` of
synapses at neuron\$connectors.

## See also

Other automatic-synapses:
[`flywire_adjacency_matrix()`](https://natverse.org/fafbseg/reference/flywire_adjacency_matrix.md),
[`flywire_ntplot()`](https://natverse.org/fafbseg/reference/flywire_ntplot.md),
[`flywire_ntpred()`](https://natverse.org/fafbseg/reference/flywire_ntpred.md),
[`flywire_partners()`](https://natverse.org/fafbseg/reference/flywire_partners.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
choose_segmentation("flywire")
nx=xform_brain(elmr::dense_core_neurons, ref="FlyWire", sample="FAFB14")
xyz = xyzmatrix(nx)
ids = unique(flywire_xyz2id(xyz[sample(nrow(xyz),100),]))
neurons = skeletor(ids, brain = elmr::FAFB14.surf)
neurons.syns = flywire_neurons_add_synapses(neurons, transmitters = TRUE)
neurons.syns[,]

# Plot in 3D
library(catmaid)
nopen3d()
plot3d(neurons.syns, WithConnectors = TRUE)

# Axon-dendrite split
library(hemibrainr)
neurons.flow = flow_centrality(neurons.syns,
  polypre = TRUE,
  mode = "centrifugal")
clear3d()
plot3d_split(neurons.flow, WithConnectors = TRUE,
transmitter = TRUE,
radius = 1000, soma = 4000)

# Save .csv of synapses as FlyWire annotations
flywire_synapse_annotations(ids[1], file="annotations1.csv",
cleft.threshold=30)

# And similar, from a neuronlist
syns = hemibrainr::hemibrain_extract_synapses(neurons.flow,
.parallel = TRUE, OmitFailures = TRUE)
flywire_synapse_annotations(syns, file="annotations2.csv",
cleft.threshold=30)

} # }
# }
```
