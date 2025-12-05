# Return a sample Neuroglancer scene URL for FlyWire dataset

Return a sample Neuroglancer scene URL for FlyWire dataset

## Usage

``` r
flywire_scene(
  ids = NULL,
  annotations = NULL,
  open = FALSE,
  shorten = FALSE,
  segmentation = "flywire31",
  ...
)
```

## Arguments

- ids:

  A set of root ids to include in the scene. Also accepts a data.frame
  containing a column `rootid`, `root_id`, `id` or any form acceptable
  to
  [`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  including neuroglancer scene URLs.

- annotations:

  data.frame or matrix of position and other information for annotation
  layers. See
  [`ngl_annotation_layers`](https://natverse.org/fafbseg/reference/ngl_annotation_layers.md)
  for details.

- open:

  Whether to open the scene in your default browser

- shorten:

  Not currently implemented

- segmentation:

  Defaults to `'flywire31'`. See
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  for other options.

- ...:

  Passed to
  [`ngl_annotation_layers`](https://natverse.org/fafbseg/reference/ngl_annotation_layers.md)

## Value

A character vector containing a single Neuroglancer URL (invisibly when
open=TRUE)

## See also

Other neuroglancer-urls:
[`flywire_ids()`](https://natverse.org/fafbseg/reference/flywire_ids.md),
[`ngl_blank_scene()`](https://natverse.org/fafbseg/reference/ngl_blank_scene.md),
[`ngl_decode_scene()`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md),
[`ngl_encode_url()`](https://natverse.org/fafbseg/reference/ngl_encode_url.md),
[`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md),
[`open_fafb_ngl()`](https://natverse.org/fafbseg/reference/open_fafb_ngl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
flywire_scene(open=T)
# top 20 partners of a neuron
flywire_scene(flywire_partner_summary("720575940621039145", partners='out')$partner[1:20], open=T)

# using the ability to query flytable for cell types
flywire_scene('DA2_lPN', open=TRUE)
flywire_scene('class:MBON_R', open=TRUE)
} # }
```
