# Extract annotations from a neuroglancer scene into a dataframe

Extract annotations from a neuroglancer scene into a dataframe

## Usage

``` r
ngl_annotations(
  x,
  layer = NULL,
  types = c("point", "line"),
  points = c("collapse", "expand", "list")
)
```

## Arguments

- x:

  A neuroglancer scene or URL (passed to
  [`ngl_decode_scene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md)
  as necessary) or a neuroglancer layers
  ([`nglayers`](https://natverse.org/fafbseg/reference/ngl_layers.md))
  extracted from such a scene.

- layer:

  Optional index vector specifying the layers within a scene from which
  to extract annotations. It is probably safest to use a character
  vector of layer names (what appears in neuroglancer). When missing all
  annotation layers are processed.

- types:

  Which annotation types to process (currently only points and lines by
  default)

- points:

  What to do with point coordinates.

## Value

A data.frame with columns defined by the contents of the annotation
layer and the `types`/`points` arguments. Additional annotation features
are stored as attributes on the data.frame.

## See also

[`ngl_annotation_layers`](https://natverse.org/fafbseg/reference/ngl_annotation_layers.md)
to make new annotation layers

## Examples

``` r
# \donttest{
u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5963849085747200"
adf=ngl_annotations(u)
str(attr(adf, 'ann_attrs'))
#> List of 1
#>  $ lines:List of 9
#>   ..$ tool                                : chr "annotatePoint"
#>   ..$ selectedAnnotation                  :List of 1
#>   .. ..$ id: chr "c8e65c47e6677d4dcb2bc2846c26ec4b277756c4"
#>   ..$ type                                : chr "annotation"
#>   ..$ annotationTags                      :List of 3
#>   .. ..$ :List of 2
#>   .. .. ..$ id   : int 1
#>   .. .. ..$ label: chr "good"
#>   .. ..$ :List of 2
#>   .. .. ..$ id   : int 2
#>   .. .. ..$ label: chr "bad"
#>   .. ..$ :List of 2
#>   .. .. ..$ id   : int 3
#>   .. .. ..$ label: chr "ok"
#>   ..$ voxelSize                           : int [1:3] 4 4 40
#>   ..$ linkedSegmentationLayer             : chr "Production-segmentation_with_graph"
#>   ..$ bracketShortcutsShowSegmentation    : logi FALSE
#>   ..$ annotationSelectionShowsSegmentation: logi FALSE
#>   ..$ name                                : chr "lines"
# }
```
