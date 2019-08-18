<!-- badges: start -->
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](http://jefferis.github.io/fafbseg/reference/)
[![Travis build status](https://travis-ci.org/jefferis/fafbseg.svg?branch=master)](https://travis-ci.org/jefferis/fafbseg)
[![Coveralls test coverage](https://coveralls.io/repos/github/jefferis/fafbseg/badge.svg)](https://coveralls.io/r/jefferis/fafbseg?branch=master)
<!-- badges: end -->

# fafbseg

The goal of fafbseg is to provide support for analysis of segmented EM data. 
This includes support for working with [neuroglancer](https://github.com/google/neuroglancer)
mesh data for the [full adult female brain dataset](http://temca2data.org/). It 
is integrated with the [NeuroAnatomy Toolbox](https://github.com/jefferis/nat)
suite especially [elmr](https://github.com/jefferis/elmr) and [catmaid](https://github.com/jefferis/rcatmaid).

## Installation

You can install the development version of fafbseg from GitHub:

``` r
# install devtools if required
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("jefferis/fafbseg")
```

## Use
### Analysis of neuroglancer meshes
Currently the package provides functionality to read neuroglancer meshes into
R and then compare such meshes with traced neuron objects e.g. from CATMAID.

At the moment there is no support for reading objects directly from a
neuroglancer scene URL. Therefore you must capture an interactive web session.
You should make sure that you only have one neuron displayed if you
do not want to have to parse the object identifier relationships.

With Chrome you can generate an appropriate set of curl download commands by:

1. Opening the Chrome Developer console (View ... Developer ... JavaScript Console),
2. (re)loading a page of interest
3. selecting the network tab
4. selecting a downloaded object 
5. right clicking and then choosing (Copy ... **Copy all as cURL**).

You can either save the contents of the clipboard into a text file (e.g. 
`all_curl.sh`) or just keep it in the clipboard.

You can then go to R and proceed as follows

```r
library(fafbseg)
# omit the first argument if you want to use the clipboard
fetch_all_curl("all_curl.sh", outdir="alldata",
  regex="brainmaps.googleapis.com", fixed=TRUE)
meshdata=read_ng_dump("alldata")
library(elmr)
y=read.neuron.catmaid(23432)
compare_ng_neuron(meshdata, y)
```

### Neuroglancer URLs

You can also use the package to generate URLs pointing to a defined location in 
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

### CATMAID URLs

You can also go in the opposite direction to convert a neuroglancer location
into a CATMAID URL. The neuroglancer location can come either from a URL or the
JSON scene specification obtained by clicking on the *{}* icon.

```r
# round trip using the CATMAID URL we used earlier
ngu = open_fafb_ngl(u, open = FALSE)

# now open using elmr package - installed like so if necessary: 
# if (!require("devtools")) install.packages("devtools") 
# devtools::install_github("jefferis/elmr")
elmr::open_fafb(ngl_decode_scene(ngu))
```
