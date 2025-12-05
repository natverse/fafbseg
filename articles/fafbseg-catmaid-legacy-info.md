# fafbseg-catmaid-legacy-info

This vignette includes information about interacting with FAFB catmaid
and Google autosegmentation services which is now principally of
historical interest.

``` r
library(fafbseg)
```

### Neuroglancer URLs

You can use the package to generate URLs pointing to a defined location
in a neuroglancer dataset. This includes arbitrary locations in the FAFB
dataset specified interactively or using CATMAID URLs. For example, we
could find the location referenced in this tweet:

> Zoom into [@dddavi](https://twitter.com/dddavi?ref_src=twsrc%5Etfw)
> whole fly brain EM dataset using public
> [@catmaid](https://twitter.com/catmaid?ref_src=twsrc%5Etfw) server
> hosted by
> [@virtualflybrain](https://twitter.com/virtualflybrain?ref_src=twsrc%5Etfw).
> From whole brain view to synaptic detail. Click to explore this
> location within the the mushroom body parallel fibre system yourself!
> <https://t.co/JxsBGe3RbJ>
> [@flyconnectome](https://twitter.com/flyconnectome?ref_src=twsrc%5Etfw)
> [pic.twitter.com/NfDiXgJOPX](https://t.co/NfDiXgJOPX)
>
> — Greg Jefferis (@gsxej) [July 24,
> 2018](https://twitter.com/gsxej/status/1021743042296983552?ref_src=twsrc%5Etfw)

``` r
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

You can use the same approach for <https://flywire.ai> URLs with the
additional option of transforming the coordinates to get closer to the
correct location in the FlyWire assembly of the FAFB data (which is
typically a few hundred nm off). See
[`flywire2fafb()`](https://natverse.org/fafbseg/reference/flywire2fafb.md)
for details.

``` r
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

You can also go in the opposite direction to convert a neuroglancer
location into a CATMAID URL. The neuroglancer location can come either
from a URL or the JSON scene specification obtained by clicking on the
*{}* icon.

``` r
# round trip using the CATMAID URL we used earlier
ngu = open_fafb_ngl(u, open = FALSE)

# now open using elmr package - installed like so if necessary: 
# natmanager::install(pkgs="natverse/elmr")
elmr::open_fafb(ngl_decode_scene(ngu))
```

### Shiny application

The package now includes a simple Shiny application that translates
between the two URL schemes. You can use an online version of the app at

<https://jefferislab.shinyapps.io/CATMAID-Neuroglancer-Converter/>

or download from GitHub and run locally the latest version of the app:

``` r
if (!require("shiny")) install.packages("shiny")
shiny::runGitHub("natverse/fafbseg", subdir = "inst/app/")
```

Currently the application does not convert FlyWire\<-\>FAFB coordinates,
but this would be a relatively simple addition using
[`flywire2fafb()`](https://natverse.org/fafbseg/reference/flywire2fafb.md).

### Analysis of neuroglancer meshes

Currently the package provides functionality to read neuroglancer meshes
into R and then compare such meshes with traced neuron objects e.g. from
CATMAID. For this you will need to authenticate with an appropriate API.
Alternatively see the article Capturing neuroglancer meshes from a
browser scene.

You can read in meshes for the [FlyWire segmentation of
FAFB](https://flywire.ai/) by doing (roughly)

``` r
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

You can read in meshes for the [Google brain segmentation of
FAFB](http://fafb-ffn1.storage.googleapis.com/landing.md) by doing
(roughly)

``` r
choose_segmentation(release = '20190805')
open_fafb_ngl(c(109459, 41305, 5424)*c(4,4,40))
va6pn=read_brainmaps_meshes("710435991")
```
