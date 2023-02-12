<!-- badges: start -->
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Release Version](https://img.shields.io/github/release/natverse/fafbseg.svg)](https://github.com/natverse/fafbseg/releases/latest) 
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](https://natverse.github.io/fafbseg/reference/)
[![R-CMD-check](https://github.com/natverse/fafbseg/workflows/R-CMD-check/badge.svg)](https://github.com/natverse/fafbseg/actions)
[![Codecov test coverage](https://codecov.io/gh/natverse/fafbseg/branch/master/graph/badge.svg)](https://app.codecov.io/gh/natverse/fafbseg?branch=master)
<!-- badges: end -->

# fafbseg

The goal of fafbseg is to provide support for analysis of segmented EM data. 
This includes support for working with [neuroglancer](https://github.com/google/neuroglancer)
mesh data for the [full adult female brain (FAFB) dataset](http://temca2data.org/). 
In particular there is support for the [FlyWire](https://flywire.ai/)
and [Google brain](http://fafb-ffn1.storage.googleapis.com/landing.html) automatic
segmentations of FAFB data.

**fafbseg** is integrated with the [NeuroAnatomy Toolbox](https://github.com/natverse/nat)
suite (aka [natverse](http://natverse.org)) especially [elmr](https://github.com/natverse/elmr) and [catmaid](https://github.com/natverse/rcatmaid).

## Installation

We recommend installing fafbseg from GitHub using the [natmanager package](http://natverse.org/natmanager/):

``` r
# install natmanager if required
if (!requireNamespace("natmanager")) install.packages("natmanager")
natmanager::install(pkgs="fafbseg")
```

## Use
### Neuroglancer URLs

You can use the package to generate URLs pointing to a defined location in 
a neuroglancer dataset. This includes arbitrary locations in the FAFB dataset 
specified interactively or using CATMAID URLs. For example, we could find the 
location referenced in this tweet:

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Zoom into <a href="https://twitter.com/dddavi?ref_src=twsrc%5Etfw">@dddavi</a> whole fly brain EM dataset using public <a href="https://twitter.com/catmaid?ref_src=twsrc%5Etfw">@catmaid</a> server hosted by <a href="https://twitter.com/virtualflybrain?ref_src=twsrc%5Etfw">@virtualflybrain</a>. From whole brain view to synaptic detail. Click to explore this location within the the mushroom body parallel fibre system yourself! <a href="https://t.co/JxsBGe3RbJ">https://t.co/JxsBGe3RbJ</a> <a href="https://twitter.com/flyconnectome?ref_src=twsrc%5Etfw">@flyconnectome</a> <a href="https://t.co/NfDiXgJOPX">pic.twitter.com/NfDiXgJOPX</a></p>&mdash; Greg Jefferis (@gsxej) <a href="https://twitter.com/gsxej/status/1021743042296983552?ref_src=twsrc%5Etfw">July 24, 2018</a></blockquote>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

```r
library(fafbseg)
# First, optionally set a package option with an example neuroglancer URL for your dataset.
# This URL will define the image layers that are visible. You need to use an
# URL for which you have access. The package ships with a default using segmentation
# fafb_v14:fafb_v14_16nm_v00c_split3xfill2 which is probably what you need.
options(fafbseg.sampleurl="https://<neuroglancerlurl>")

# Now open location specified by CATMAID URL
u='https://fafb.catmaid.virtualflybrain.org/?pid=2&zp=131280&yp=170014.98879622458&xp=426584.81386896875&tool=navigator&sid0=2&s0=-1'
open_fafb_ngl(u)

# or display raw coordinates that you can paste into active Neuroglancer session
open_fafb_ngl(u, coords.only = TRUE)
```

You can use the same approach for https://flywire.ai URLs with the additional
option of transforming the coordinates to get closer to the correct location in 
the FlyWire assembly of the FAFB data (which is typically a few hundred nm off).
See `flywire2fafb()` for details.

```r
# opens a new browser tab
with_segmentation('flywire31', 
  open_fafb_ngl(u, sample = 'FAFB14', reference = "FlyWire"))

# writes coordinates to clipboard so that you can paste them into an existings
# flywire neuroglancer browser tab
with_segmentation('flywire31', 
  clipr::write_clip(
    open_fafb_ngl(u, coords.only = TRUE, 
      sample = 'FAFB14', reference = "FlyWire")))
```

### CATMAID URLs

You can also go in the opposite direction to convert a neuroglancer location
into a CATMAID URL. The neuroglancer location can come either from a URL or the
JSON scene specification obtained by clicking on the *{}* icon.

```r
# round trip using the CATMAID URL we used earlier
ngu = open_fafb_ngl(u, open = FALSE)

# now open using elmr package - installed like so if necessary: 
# natmanager::install(pkgs="natverse/elmr")
elmr::open_fafb(ngl_decode_scene(ngu))
```
### Shiny application

The package now includes a simple Shiny application that translates between the
two URL schemes. You can use an online version of the app at 

https://jefferislab.shinyapps.io/CATMAID-Neuroglancer-Converter/

or download from GitHub and run locally the latest version of the app:

```r
if (!require("shiny")) install.packages("shiny")
shiny::runGitHub("natverse/fafbseg", subdir = "inst/app/")
```

Currently the application does not convert FlyWire<->FAFB coordinates, but this
would be a relatively simple addition using `flywire2fafb()`.

### Analysis of neuroglancer meshes
Currently the package provides functionality to read neuroglancer meshes into
R and then compare such meshes with traced neuron objects e.g. from CATMAID. 
For this you will need to authenticate with an appropriate API. Alternatively
see the article Capturing neuroglancer meshes from a browser scene.

You can read in meshes for the [FlyWire segmentation of FAFB](https://flywire.ai/)
by doing (roughly)

```r
choose_segmentation(release = 'flywire31')
open_fafb_ngl(c(109459, 41305, 5424)*c(4,4,40))
va6pn=read_cloudvolume_meshes("720575940633169983")
# compare with VFB CATMAID
library(elmr)
va6pn.skel=read.neurons.catmaid("name:VA6.*PN")
# let's take a look at those
plot3d(va6pn, col='grey', lwd=0.5, type='wire')
# only plot first neuron on RHS
plot3d(va6pn.skel[1], lwd=3, col='red')

# can also transform the FAFB14 skeleton from VFB to FlyWire coordinates
va6pn.skel.fw <- xform_brain(va6pn.skel[1], reference = 'FlyWire', sample='FAFB14')
nclear3d()
plot3d(va6pn, col='grey', lwd=0.5, type='wire')
# only plot first neuron on RHS
plot3d(va6pn.skel.fw, lwd=3, col='red')
```

You can read in meshes for the [Google brain segmentation of FAFB](http://fafb-ffn1.storage.googleapis.com/landing.html) by doing (roughly)

```r
choose_segmentation(release = '20190805')
open_fafb_ngl(c(109459, 41305, 5424)*c(4,4,40))
va6pn=read_brainmaps_meshes("710435991")
```

## Acknowledgements

The fafbseg package enables access to a number of published and many pre-publication 
resources. We hope that this will accelerate your science but we **strongly**
request that you ensure that you acknowledge both the authors of this package
and the original data sources to ensure that we and they can justify this free
sharing of code and data.

* For the FAFB dataset, [Zheng et al Cell 2018](https://www.cell.com/cell/fulltext/S0092-8674(18)30787-6)
* For the FFN1 autosegmentation, [Li et al bioRxiv 2019](https://www.biorxiv.org/content/10.1101/605634v3)
* For the FlyWire autosegmentation, [Dorkenwald et al Nat Meth 2022](https:// https://doi.org/10.1038/s41592-021-01330-0) and https://flywire.ai
* For the Buhmann synaptic connection autosegmentation, [Buhmann et al Nat Meth 2021](https://doi.org/10.1038/s41592-021-01183-7)
  * if using the `cleft_score` metric please also cite [Heinrich et al 2018](https://link.springer.com/chapter/10.1007%2F978-3-030-00934-2_36) 
* For the neurotransmitter prediction, [Eckstein et al. 2020 bioRxiv](https://www.biorxiv.org/content/10.1101/2020.06.12.148775v1)

FlyWire coordinate transforms and synapse predictions make use of infrastructure
contributed by Davi Bock, Gregory Jefferis, Philipp Schlegel and Eric Perlman,
supported by NIH BRAIN Initiative (grant 1RF1MH120679-01); additional work
including assembling ground truth data was also supported by Wellcome trust
(203261/Z/16/Z). 

For the fabseg package itself, please include a reference to the github page
in your methods section as well as a citation to [Bates et al eLife 2020](https://doi.org/10.7554/eLife.53350) along with the statement:

> Development of the natverse including the fafbseg package has been supported 
by the NIH BRAIN Initiative (grant 1RF1MH120679-01), NSF/MRC Neuronex2 (NSF 2014862/MC_EX_MR/T046279/1) and core funding from the Medical Research Council (MC_U105188491).

**Thank you!**
