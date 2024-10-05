<!-- badges: start -->
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Release Version](https://img.shields.io/github/release/natverse/fafbseg.svg)](https://github.com/natverse/fafbseg/releases/latest) 
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](https://natverse.github.io/fafbseg/reference/)
[![R-CMD-check](https://github.com/natverse/fafbseg/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/natverse/fafbseg/actions)
[![Codecov test coverage](https://codecov.io/gh/natverse/fafbseg/branch/master/graph/badge.svg)](https://app.codecov.io/gh/natverse/fafbseg?branch=master)
<!-- badges: end -->

# fafbseg

The goal of fafbseg is to provide support for analysis of segmented EM data,
focussed on the [full adult female brain (FAFB) dataset](http://temca2data.org/).

Although there is support for a range of data sources and services, at this point
the principal target is the [FlyWire](https://flywire.ai/) automated segmentation.
Legacy support also exists for the [Google brain](http://fafb-ffn1.storage.googleapis.com/landing.html) automatic
segmentation of FAFB data.

**fafbseg** is integrated with the [NeuroAnatomy Toolbox](https://github.com/natverse/nat)
suite (aka [natverse](http://natverse.org)) including [elmr](https://github.com/natverse/elmr) and [catmaid](https://github.com/natverse/rcatmaid).

**fafbseg** is also one building block for the [coconatfly](https://natverse.org/coconatfly/)
which provides a unified and simplified interface to a range of Drosophila
connectome datasets. We actually recommend [coconatfly](https://natverse.org/coconatfly/)
as a good place to start for most users since it provides convenient access
to specific FlyWire data releases as well as powerful cross-connectome analyses
while requiring minimal configuration. See
https://natverse.org/coconatfly/articles/getting-started.html for details.

## Installation

Assuming you want to use the production (in progress) version of the FlyWire dataset
or to access lower level functionality, then fafbseg is the way to go. You will
need to install the package and then configure your environment including
recording your authorisation token and likely downloading canned data releases.

### Package installation

We recommend installing fafbseg from GitHub using the [natmanager package](http://natverse.org/natmanager/):

``` r
# install natmanager if required
if (!requireNamespace("natmanager")) install.packages("natmanager")
natmanager::install(pkgs="fafbseg")
```

### FlyWire setup

Basic steps for setting up to access FlyWire data:

``` r
library(fafbseg)
# record your authorisation token
flywire_set_token()
# install python tools required for some functionality
simple_python()
# fetch canned connectivity *and* cell type data 
download_flywire_release_data()
```
note that at the time of writing (Dec 2023) `download_flywire_release_data()` 
targets materialisation 783 of the FlyWire dataset, fetching both annotations and
connectivity information.

## Use

Detailed examples will follow in additional vignettes, but as a first motivation,
this is how to do a connectivity query using precomputed data.
``` r
library(fafbseg)
dl4df=flytable_meta('DL4.*')
dl4df

dl4out <- flywire_partner_summary2(dl4df, partners = 'out', threshold = 3)
dl4out
```
## Acknowledgements

The fafbseg package enables access to a number of published and many pre-publication 
resources. We hope that this will accelerate your science but we **strongly**
request that you ensure that you acknowledge both the authors of this package
and the original data sources to ensure that we and they can justify this free
sharing of code and data.

For use of the proofread and annotated FlyWire dataset, please co-cite:

* [Dorkenwald et al 2023](https://doi.org/10.1101/2023.06.27.546656)
* [Schlegel et al. 2023](https://doi.org/10.1101/2023.06.27.546055)

To acknowledge specific FAFB resources:

* For the FAFB dataset, [Zheng et al Cell 2018](https://www.cell.com/cell/fulltext/S0092-8674(18)30787-6)
* For the FFN1 autosegmentation, [Li et al bioRxiv 2019](https://www.biorxiv.org/content/10.1101/605634v3)
* For the FlyWire autosegmentation, [Dorkenwald et al Nat Meth 2022](https:// https://doi.org/10.1038/s41592-021-01330-0) and https://flywire.ai
* For the Buhmann synaptic connection autosegmentation, [Buhmann et al Nat Meth 2021](https://doi.org/10.1038/s41592-021-01183-7)
  * if using the `cleft_score` metric please also cite [Heinrich et al 2018](https://link.springer.com/chapter/10.1007%2F978-3-030-00934-2_36) 
* For the neurotransmitter prediction, [Eckstein et al. 2023 bioRxiv](https://www.biorxiv.org/content/10.1101/2020.06.12.148775)

FlyWire coordinate transforms and synapse predictions make use of infrastructure
contributed by Davi Bock, Gregory Jefferis, Philipp Schlegel and Eric Perlman,
supported by NIH BRAIN Initiative (grant 1RF1MH120679-01); additional work
including assembling ground truth data was also supported by Wellcome trust
(203261/Z/16/Z). 

For the fafbseg package itself, please include a reference to the github page
in your methods section as well as a citation to [Bates et al eLife 2020](https://doi.org/10.7554/eLife.53350) along with the statement:

> Development of the natverse including the fafbseg package has been supported 
by the NIH BRAIN Initiative (grant 1RF1MH120679-01), NSF/MRC Neuronex2 (NSF 2014862/MC_EX_MR/T046279/1) and core funding from the Medical Research Council (MC_U105188491).

**Thank you!**
