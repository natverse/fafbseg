# Add a transformation to one or more layers in a neuroglancer scene

Add a transformation to one or more layers in a neuroglancer scene

## Usage

``` r
# S3 method for class 'ngscene'
xform(x, reg, layers = NULL, ...)
```

## Arguments

- x:

  A neuroglancer scene as produced by
  [`ngl_decode_scene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md)

- reg:

  A registration either as a
  [`reglist`](https://rdrr.io/pkg/nat/man/reglist.html) containing
  multiple registrations or a single registration in any form handled by
  [`xform`](https://rdrr.io/pkg/nat/man/xform.html).

- layers:

  A character vector specifying the layers in the scene to transform. If
  the elements are named, they names specify the *new* names of the
  transformed layer.

- ...:

  Additional arguments passed to `fit_xform` when `reg` specifies a
  non-rigid registration. See **details**.

## Value

A new `ngscene` object

## Details

Neuroglancer only implements homogeneous affine transforms for layers.
However these can still be quite useful when a non-rigid transform
cannot be applied to a layer e.g. because the underlying neurons are
undergoing rapid editing and it is not practical to generate a static
set of transformed meshes.

## Examples

``` r
# \donttest{
# flywire scene
u='https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/4559706743898112'
scf=ngl_decode_scene(u)
m=matrix(c(-0.9663, -0.0695, 0.17, 0, 0.0351, 1.043, -0.028, 0, 0.1082,
  -0.0093, 0.9924, 0, 1021757.1284, 31409.0911, -85626.0572, 1), ncol=4)
# nb replaces existing layer of this name
xform(scf, m, layers=c('Production-mirrored'))
#> neuroglancer scene with 4 layers and 1 segments (of which 1 shown)
#>                   name                    type archived visible nsegs nhidden
#> 1       Maryland-image                   image    FALSE    TRUE     0       0
#> 2           Production segmentation_with_graph    FALSE    TRUE     1       0
#> 3  Production-mirrored segmentation_with_graph    FALSE    TRUE     1       0
#> 4 brain_mesh_v141.surf            segmentation    FALSE    TRUE     1       0
#>                                                                           source
#> 1        precomputed://https://bossdb-open-data.s3.amazonaws.com/flywire/fafbv14
#> 2             graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31
#> 3             graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31
#> 4 precomputed://gs://flywire_neuropil_meshes/whole_neuropil/brain_mesh_v141.surf

scu=ngl_decode_scene('https://tinyurl.com/kj9rwn26')
# make a new layer mirroring an existing layer
scu2=xform(scu, m, layers=c('fly_v31_m'='fly_v31'))
scu2
#> neuroglancer scene with 4 layers and 2 segments (of which 2 shown)
#>              name         type archived visible nsegs nhidden
#> 1              v1        image    FALSE    TRUE     0       0
#> 2         fly_v31 segmentation    FALSE    TRUE     2       0
#> 3 jfrc_mesh_test1 segmentation    FALSE   FALSE     1       0
#> 4       fly_v31_m segmentation    FALSE    TRUE     2       0
#>                                                                        source
#> 1                                    precomputed://gs://flywire_em/aligned/v1
#> 2 graphene://middleauth+https://prod.flywire-daf.com/segmentation/1.0/fly_v31
#> 3             precomputed://https://spine.itanna.io/files/eric/jfrc_mesh_test
#> 4 graphene://middleauth+https://prod.flywire-daf.com/segmentation/1.0/fly_v31
# }
if (FALSE) { # \dontrun{
browseURL(as.character(scu2))
} # }

if (FALSE) { # \dontrun{
# mirror a flywire scene based on points from a specific pair of neurons
mbon18.dps=read_l2dp('MBON18')
mirror_reg=fit_xform(samplepts = mbon18.dps,
  refpts = nat.jrcbrains::mirror_fafb(mbon18.dps), subsample = 500)
flywire_scene('MBON18') %>%
  ngl_decode_scene %>%
  xform(mirror_reg, layers=c("mirror"="Production-segmentation_with_graph")) %>%
  as.character() %>%
  browseURL()
} # }
```
