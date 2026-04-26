# Return raw neurotransmitter prediction results for output of flywire neuron

the `print.ntprediction` method provides a quick summary of the
neurotransmitter prediction for all output synapses.

## Usage

``` r
flywire_ntpred(
  x,
  cleft.threshold = 0,
  remove_autapses = TRUE,
  local = NULL,
  cloudvolume.url = NULL
)

# S3 method for class 'ntprediction'
print(x, ...)
```

## Arguments

- x:

  A single root id as a string OR a `data.frame` of output (downstream)
  partners returned by `flywire_partners`.

- cleft.threshold:

  A threshold for the cleft score calculated by Buhmann et al 2019
  (default 0, we have used 30-100 to increase specificity)

- remove_autapses:

  For `flywire_partner_summary` whether to remove autapses (defaults to
  TRUE)

- local:

  path to SQLite synapse data. Evaluated by `fafbseg:::local_or_google`.
  Work in progress. Default is to download this data and place it in
  `~/projects/JanFunke`.

- cloudvolume.url:

  The segmentation source URL for cloudvolume. Normally you can ignore
  this and rely on the default segmentation chosen by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)

- ...:

  additional arguments passed to
  [`print`](https://rdrr.io/r/base/print.html)

## Value

A `data.frame` of neurotransmitter predictions

## See also

Other automatic-synapses:
[`flywire_adjacency_matrix()`](https://natverse.org/fafbseg/reference/flywire_adjacency_matrix.md),
[`flywire_neurons_add_synapses()`](https://natverse.org/fafbseg/reference/flywire_neurons_add_synapses.md),
[`flywire_ntplot()`](https://natverse.org/fafbseg/reference/flywire_ntplot.md),
[`flywire_partners()`](https://natverse.org/fafbseg/reference/flywire_partners.md)

## Examples

``` r
# \donttest{
# an olfactory projection neuron
flywire_ntpred("720575940615237849")
#> Warning: /home/runner/projects/JanFunke//flywire_synapses.db does not exist
#> Warning: /home/runner/projects/JanFunke//20191211_fafbv14_buhmann2019_li20190805_nt20201223.db does not exist
#> neuron 720575940615237849 with 23081 output synapses:
#> acetylcholine     serotonin      dopamine          gaba     glutamate 
#>        94.658         2.158         1.447         0.737         0.607 
#>    octopamine 
#>         0.394 
# alternatively
if (FALSE) { # \dontrun{
flywire_ntpred(flywire_xyz2id(cbind(116923, 61378, 1474), rawcoords = T))
} # }
# }
```
