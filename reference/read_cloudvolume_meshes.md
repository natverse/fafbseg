# Read meshes from chunked graph (graphene) server via CloudVolume

`save_cloudvolume_meshes` saves meshes to disk.

`read_cloudvolume_meshes` uses `save_cloudvolume_meshes` internally to
save meshes to disk and then reads them into memory as a
[`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html).

## Usage

``` r
save_cloudvolume_meshes(
  segments,
  savedir = tempfile(),
  OmitFailures = TRUE,
  Force = FALSE,
  format = c("obj", "ply"),
  ...,
  cloudvolume.url = getOption("fafbseg.cloudvolume.url")
)

read_cloudvolume_meshes(
  segments,
  savedir = NULL,
  ...,
  cloudvolume.url = getOption("fafbseg.cloudvolume.url")
)
```

## Arguments

- segments:

  The segment ids to fetch (probably as a character vector)

- savedir:

  Optional path to a directory in which obj format files will be stored.
  If not specified, a temporary directory will be created and removed at
  the end of the call.

- OmitFailures:

  Whether to omit neurons for which `FUN` gives an error. The default
  value (`NA`) will result in nlapply stopping with an error message the
  moment there is an error. For other values, see details.

- Force:

  whether to overwrite a downloaded mesh of the same name

- format:

  whether to save meshes in Wavefront obj or Stanford poly format. obj
  is the default but ply is a simpler and more compact format.

- ...:

  Additional arguments passed to `save_cloudvolume_meshes` and then
  eventually to the Python CloudVolume constructor (see
  <https://github.com/seung-lab/cloud-volume> for details.

- cloudvolume.url:

  Optional url from which to fetch meshes normally specified by the
  `fafbseg.cloudvolume.url` option.

## Value

A
[`rgl::shapelist3d`](https://dmurdoch.github.io/rgl/dev/reference/shapelist3d.html)
list containing one or more `mesh3d` objects named by the segment id.

## Details

You may to use this to fetch meshes from <https://flywire.ai> among
other sources. You may need to select your preferred remote data source
using
[`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
(see examples). Under the hood, it uses the
[CloudVolume](https://github.com/seung-lab/cloud-volume) serverless
Python client for reading data in
[Neuroglancer](https://github.com/google/neuroglancer/) compatible
formats. You will therefore need to have a working python3 install of
CloudVolume.

Please install the Python CloudVolume module as described at:
<https://github.com/seung-lab/cloud-volume#setup>. You must ensure that
you are using python3 (implicitly or explicitly) as mesh fetching from
graphene servers depends on this. This should normally work:
`pip3 install cloud-volume`. If you have already installed CloudVolume
but it is not found, then I recommend editing your
[`Renviron`](https://rdrr.io/r/base/Startup.html) file to set an
environment variable pointing to the correct Python. You can do this
with
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)
and then setting e.g. `RETICULATE_PYTHON="/usr/local/bin/python3"`.

You will normally need to set up some kind of authentication in order to
fetch data. For flywire, we recommend the function
[`flywire_set_token`](https://natverse.org/fafbseg/reference/flywire_set_token.md).
For other data sources or more details, see
<https://github.com/seung-lab/cloud-volume#chunkedgraph-secretjson> for
how to get a token and where to save it. You can either save a json
snippet to `~/.cloudvolume/secrets/cave-secret.json` or set an
environment variable (`CHUNKEDGRAPH_SECRET="XXXX"`.

Finally you will also need to set an option pointing to your server.
This is most conveniently achieved using e.g.
`choose_segmentation('flywire31')`, which is now the default, but for
sources without built-in support, you can also specify a full source
URL, which might look something like

`options(fafbseg.cloudvolume.url='graphene://https://xxx.dynamicannotationframework.com/segmentation/xxx/xxx')`

You can easily add this to your startup
[`Rprofile`](https://rdrr.io/r/base/Startup.html) with
[`usethis::edit_r_profile()`](https://usethis.r-lib.org/reference/edit.html).

## See also

[`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md).
See
[`simple_python`](https://natverse.org/fafbseg/reference/simple_python.md)
for installation of the necessary Python packages.

## Examples

``` r
if (FALSE) { # \dontrun{
kcmesh=save_cloudvolume_meshes("720575940623755722", savedir=".")
kc=read.neurons(kcmesh)
} # }
if (FALSE) { # \dontrun{
# The very first time you access FlyWire data you need to get/store a token
flywire_set_token()

# Each R session, you should choose the default segmentation you want
choose_segmentation('flywire31')
pmn1.flywire=read_cloudvolume_meshes("720575940623979522")
pmn1.fafb=read.neuron.catmaid(5321581)

# Read and plot sample KCs from a FlyWire (short) URL
u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/6230669436911616"
kcs=read_cloudvolume_meshes(u)
kcs
plot3d(kcs)

nclear3d()
plot3d(pmn1.fafb, col='red', lwd=2, WithNodes = F)
wire3d(pmn1.flywire)

# you can select specific locations like so
library(elmr)
# CATMAID URL
open_fafb(pmn1.flywire[[1]], open=F)
# CATMAID coords to paste into PIN location box
cat(xyzmatrix(catmaid::catmaid_parse_url(open_fafb(pmn1.flywire[[1]], open=F))), sep=',')
# Neuroglancer coords (raw pixels not nm)
open_fafb_ngl(pmn1.flywire[[1]], open=F, coords.only = T)
} # }
```
