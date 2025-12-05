# Construct one or more neuroglancer annotation layers

Construct one or more neuroglancer annotation layers

## Usage

``` r
ngl_annotation_layers(ann, rawcoords = NA, colpal = NULL)
```

## Arguments

- ann:

  An annotation dataframe (see details) or any object containing 3D
  vertices from which
  [`xyzmatrix`](https://natverse.org/fafbseg/reference/xyzmatrix.md) can
  successfully extract points.

- rawcoords:

  Whether points have been provided in raw (voxel) coordinates or in
  calibrated (nm) positions. The default of `NA` will try to infer this
  based on the coordinate values but see details for limitations.

- colpal:

  A function or named character vector of colours that will be used to
  set the colour for each layer. Colours should be specified by name or
  hex format.

## Value

A list of additional class `nglayers` which can be added to an `ngscene`
object as produced by
[`ngl_decode_scene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md).

## Details

If you supply a dataframe for the `ann` argument then you can have
columns called

- `point` or `position` or `pt_position` to define the position. This
  should contain x,y,z coordinates formatted as a character vector
  ([`xyzmatrix2str`](https://rdrr.io/pkg/nat/man/xyzmatrix.html)) or a
  `list` of `numeric` `vector`s
  ([`xyzmatrix2list`](https://rdrr.io/pkg/nat/man/xyzmatrix.html)).

- `layer` optionally name a layer for each point

- `col` optionally specify a color for each point.

- `root_id` optionally specify a supervoxel id that the point maps onto

- `supervoxel_id` optionally specify a supervoxel id that the point maps
  onto

Neuroglancer only allows one colour per annotation layer, so if you
specify both `col` and `layer` they must be consistent.

Neuroglancer annotations are specified in raw coordinates. Although this
function can try to convert nm coordinates to raw, this will only work
for points in the brain space defined by the current fafb segmentation
(see
[`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)).
For this reason you should used `rawcoords=FALSE` and convert
coordinates yourself if you are working with other brain spaces.

## See also

[`ngl_annotations`](https://natverse.org/fafbseg/reference/ngl_annotations.md)
to extract annotations from a scene.

## Examples

``` r
if (FALSE) { # \dontrun{
## as an example label proofread neurons by institution
psp=flywire_cave_query('proofreading_status_public_v1')
fwusers=googlesheets4::read_sheet('1G0zqA5DTrfd-a2LuebV4kcqNfl4q1ehlzHBrwT6ZMoc')
psp2=dplyr::left_join(psp, fwusers, by=c("user_id"="id"))
psp2$layer=psp2$institution
# sample 3000 neurons to be a more manageable as an example.
psp2s=dplyr::slice_sample(psp2, n=3000) %>%
  dplyr::filter(!is.na(layer))
# the layers will be rainbow coloured
al=ngl_annotation_layers(psp2s[c("pt_position", "layer")], colpal=rainbow)
# make a blank scene
sc=ngl_blank_scene()
# or decode a URL that you've copied from your browser
sc=ngl_decode_scene(clipr::read_clip())
# and the add your annotations as new layer(s) to that scene
sc2=sc+al
# and make a URL
u=as.character(sc2)
# and copy that to clipboard
clipr::write_clip(u)
# ... or open directly in your browser
browseURL(u)
# It is a good idea to shorten when there are many annotations.
# This will load much faster in the browser and be easier to work with
su=flywire_shortenurl(u)
browseURL(su)
} # }
```
