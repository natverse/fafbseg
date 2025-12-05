# Fetch the synaptic adjacency matrix for a set of flywire neurons

Get an adjacency matrix for the predicted synaptic connectivity within a
set of specific flywire bodies. You can specify a single pool of ids or
separate input (upstream) and output (downstream) ids. In contrast to
[`flywire_partner_summary`](https://natverse.org/fafbseg/reference/flywire_partners.md)
this only returns connections amongst a defined set of ids rather than
all possible partners.

## Usage

``` r
flywire_adjacency_matrix(
  rootids = NULL,
  inputids = NULL,
  outputids = NULL,
  sparse = FALSE,
  remove_autapses = TRUE,
  cleft.threshold = 0,
  Verbose = interactive(),
  method = c("auto", "spine", "sqlite")
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

- remove_autapses:

  whether to remove autapses (self-connections); most of these are
  erroneous.

- cleft.threshold:

  A threshold for the cleft score calculated by Buhmann et al 2019
  (default 0, we have used 30-100 to increase specificity)

- Verbose:

  Logical indication whether to print status messages during the query
  (default `T` when interactive, `F` otherwise).

- method:

  Whether to use a local SQLite database or remote spine service for
  synapse data. The default `auto` uses a local database when available
  (45GB but faster).

## Value

A matrix with named rows of inputs and columns of outputs. The matrix
will be square when rootids is specified but may otherwise be
rectangular. Defaults to a regular (dense) matrix unless `sparse=TRUE`.

## Limitations

This function is currently much more efficient when local SQLite tables
are available; in their absence queries to the remote *spine* server are
possible but currently transfer more data than necessary. Future work
could allow *spine* queries than consider both pre and postsynaptic
supervoxel ids as part of the query.

You should also be careful about how many neurons you attempt to query.
The function is not designed to handle queries involving hundreds of
neurons with the spine method being especially sensitive to overloading.
If this is your intention, you might be better off using
[`flywire_partners`](https://natverse.org/fafbseg/reference/flywire_partners.md)
or
[`flywire_partner_summary`](https://natverse.org/fafbseg/reference/flywire_partners.md)
both of which fetch data in chunks and then manually filtering down to
your ensemble of interest.

## Normalisation

It is always important to give careful thought to data normalisation
when analysing these connectivity matrices. In general we feel that
normalising by the total input onto each target cell makes the most
sense, since this approximates the effectiveness of input in making the
target cell fire. However if you do not include all inputs onto the
target cells then even this normalisation has difficulties and it may be
better to use raw counts.

## See also

Other automatic-synapses:
[`flywire_neurons_add_synapses()`](https://natverse.org/fafbseg/reference/flywire_neurons_add_synapses.md),
[`flywire_ntplot()`](https://natverse.org/fafbseg/reference/flywire_ntplot.md),
[`flywire_ntpred()`](https://natverse.org/fafbseg/reference/flywire_ntpred.md),
[`flywire_partners()`](https://natverse.org/fafbseg/reference/flywire_partners.md)

## Examples

``` r
# \donttest{
u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5392055178100736"
sm=flywire_adjacency_matrix(u)
#> Warning: /home/runner/projects/JanFunke//flywire_synapses.db does not exist
#> Warning: /home/runner/projects/JanFunke//20191211_fafbv14_buhmann2019_li20190805_nt20201223.db does not exist
# scaled to give proportion of inputs onto each target cell
heatmap(sm, scale='col')

# scale='none' => raw counts
# nb note use of assignment and keep.dendro so we can use dendrogram later
h=heatmap(sm, scale='none', keep.dendro = TRUE)

# same but with the cleft threshold applied
smc=flywire_adjacency_matrix(u, cleft.threshold = 30)
#> Warning: /home/runner/projects/JanFunke//flywire_synapses.db does not exist
#> Warning: /home/runner/projects/JanFunke//20191211_fafbv14_buhmann2019_li20190805_nt20201223.db does not exist
# note the reuse of the earlier dendrogram to return col order for comparison
heatmap(smc, scale='none', Colv=h$Colv)

# just a single upstream neuron
sm2=flywire_adjacency_matrix(inputids="720575940625862385", outputids=u)
#> Warning: /home/runner/projects/JanFunke//flywire_synapses.db does not exist
#> Warning: /home/runner/projects/JanFunke//20191211_fafbv14_buhmann2019_li20190805_nt20201223.db does not exist
# }
```
