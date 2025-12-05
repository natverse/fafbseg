# Read L2 skeleton or dotprops for FlyWire data sources using fafbseg-py

`read_l2skel` reads one or more neurons as simplified L2 skeletons.

`read_l2dp` reads one or more neurons as simplified dotprops format. See
details.

## Usage

``` r
read_l2skel(id, OmitFailures = TRUE, datastack_name = NULL, ...)

read_l2dp(id, OmitFailures = TRUE, datastack_name = NULL, ...)
```

## Arguments

- id:

  One or more flywire ids

- OmitFailures:

  Whether or not to drop neurons that cannot be read from the results
  (rather than erroring out). Default `TRUE`.

- datastack_name:

  A CAVE datastack_name. When missing will use the default implied by
  the segmentation chosen by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md).

- ...:

  Additional arguments passed to the `fafbseg.flywire.l2_skeleton` or
  `fafbseg.flywire.l2_dotprops`functions.

## Details

`read_l2dp` is generally recommended rather than fetching a skeleton and
then calculating dotprops because it is much faster and also computes
better direction vectors. However if you wish to simplify a skeleton
(e.g. to find the cell body fibre) then you will need to take the two
step approach. This also has the possible advantage that you can specify
the step size at which direction vectors are generated along the neuron.
Note also that `read_l2dp` may drop some regions of the neuron (likely
thin ones) that define only a very small mesh volume.

These functions depends on Philipp Schlegel's `fafbseg-py` package. You
can install this using
[`simple_python`](https://natverse.org/fafbseg/reference/simple_python.md).

The `datastack_name` argument is optional because the correct datastack
name and corresponding cloud volume URL will be read from options set by
[`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md);
this is generally the preferred way for end users to select an active
dataset. Neverthless, if a `datastack_name` it will be used to look up
the correct segmentation URL and fafbseg-py will be correctly set up
using these two pieces of information.

## Examples

``` r
if (FALSE) { # \dontrun{
# install full set of recommended packages including fafbseg-py
simple_python("full")
kcsvids=c("78603674556915608", "78462662124123765", "77547662357982001",
"78533168373869635", "78251418452635714", "78323024281482155",
"78322062208411707", "78533649477402370", "77829412279715493",
"77899643517979532", "78814230967028270", "78533993141739277",
"78041274292494941", "78252449311896359", "77618924522629940",
"77618237260576979", "78673768356594679", "78182148951479619",
"78392293379997680", "77688812230426430")
kcids=flywire_rootid(kcsvids)
kcs=read_l2skel(kcids)

library(nat.nblast)
kcdps=read_l2dp(kcids)
# nb these are in microns
boundingbox(kcdps)
kcaba=nblast_allbyall(kcdps)
kchc=nhclust(scoremat = kcaba)
plot(kchc)
# 3d plot using the skeletons rather than dotprops versions of the neurons
# gamma neurons seprate from the rest
plot3d(kchc, k=2, db=kcs)
} # }
```
