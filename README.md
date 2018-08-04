[![Travis build status](https://travis-ci.org/jefferis/fafbseg.svg?branch=master)](https://travis-ci.org/jefferis/fafbseg)

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

Currently the package provides functionality to read neuroglancer meshes into
R and then comparse such meshes with traced neuron objects.

At the moment there is no support for reading objects directly from a
neuroglancer scene URL. Therefore you must capture an interactive web session.
You should make sure that you only have one neuron displayed if you
do not want to have to parse the object identifier relationships.

With Chrome you can generate an appropriate set of curl download commands by
opening the Chrome Developer console (View ... Developer ... JavaScript Console),
(re)loading a page of interest, selecting the network tab, selecting a
downloaded object, right clicking and then choosing (Copy ... Copy all as
cURL).

You can then go to R and proceed as follows

```r
library(fafbseg)
fetch_all_curl("all_curl.sh", outdir="alldata",
  regex="brainmaps.googleapis.com", fixed=TRUE)
meshdata=read_ng_dump("alldata")
library(elmr)
y=read.neuron.catmaid(23432)
compare_ng_neuron(meshdata, y)
```
