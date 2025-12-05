# Encode scene information into a neuroglancer URL

`ngl_encode_url` converts an R list containing a neuroglancer scene into
a URL that you can open in your browser.

`as.character.ngscene` is another way to convert a neuroglancer scene
object to a URL.

## Usage

``` r
ngl_encode_url(body, baseurl = NULL, auto_unbox = TRUE, ...)

# S3 method for class 'ngscene'
as.character(x, ...)
```

## Arguments

- body:

  A text file or character vector with JSON data or an R list object of
  class `ngscene`.

- baseurl:

  A URL specifying the neuroglancer server (if missing, uses the URL
  from which `body` was decoded if that was recorded or, failing that,
  `options("fafbseg.sampleurl")`). You can use any neuroglancer URL as
  will be appropriately truncated if it encodes scene information.

- auto_unbox:

  For expert use only. See
  [`toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  for details.

- ...:

  Additional arguments for
  [`toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)

- x:

  the `ngscene` object to be converted to a URL

## Value

Character vector containing encoded URL

## Details

We take pains to ensure that entries that neuroglancer expects to be
JSON arrays are (including `segments` and `hiddenSegments`) are always
mapped to a JSON array (even when length 1).

The default baseurl depends on the current segmentation chosen by
[`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md).

## See also

[`URLencode`](https://rdrr.io/r/utils/URLencode.html),
[`open_fafb_ngl`](https://natverse.org/fafbseg/reference/open_fafb_ngl.md),
[`toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)

Other neuroglancer-urls:
[`flywire_ids()`](https://natverse.org/fafbseg/reference/flywire_ids.md),
[`flywire_scene()`](https://natverse.org/fafbseg/reference/flywire_scene.md),
[`ngl_blank_scene()`](https://natverse.org/fafbseg/reference/ngl_blank_scene.md),
[`ngl_decode_scene()`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md),
[`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md),
[`open_fafb_ngl()`](https://natverse.org/fafbseg/reference/open_fafb_ngl.md)

## Examples

``` r
# \donttest{
# get sample FlyWire URL
fw_url=with_segmentation('flywire31', getOption('fafbseg.sampleurl'))
# only a 0 (dummy) segment id present
ngl_segments(fw_url)
#> [1] "0"
#
fw_sc=ngl_decode_scene(fw_url)
# add a segment
fw_sc$layers[[2]]$segments=union(fw_sc$layers[[2]]$segments,
  "720575940626877799")
# convert back to a URL, nb this depends on choose_segmentation
ngl_encode_url(fw_sc)
#> [1] "https://ngl.flywire.ai/#!%7B%22layers%22%3A%5B%7B%22source%22%3A%22precomputed%3A%2F%2Fgs%3A%2F%2Fmicrons-seunglab%2Fdrosophila_v0%2Falignment%2Fimage_rechunked%22%2C%22type%22%3A%22image%22%2C%22blend%22%3A%22default%22%2C%22shaderControls%22%3A%7B%7D%2C%22name%22%3A%22Production-image%22%7D%2C%7B%22source%22%3A%22graphene%3A%2F%2Fhttps%3A%2F%2Fprod.flywire-daf.com%2Fsegmentation%2Ftable%2Ffly_v31%22%2C%22type%22%3A%22segmentation_with_graph%22%2C%22segments%22%3A%5B%220%22%2C%22720575940626877799%22%5D%2C%22skeletonRendering%22%3A%7B%22mode2d%22%3A%22lines_and_points%22%2C%22mode3d%22%3A%22lines%22%7D%2C%22graphOperationMarker%22%3A%5B%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%5D%2C%22pathFinder%22%3A%7B%22color%22%3A%22%23ffff00%22%2C%22pathObject%22%3A%7B%22annotationPath%22%3A%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%22hasPath%22%3Afalse%7D%7D%2C%22name%22%3A%22Production-segmentation_with_graph%22%7D%5D%2C%22navigation%22%3A%7B%22pose%22%3A%7B%22position%22%3A%7B%22voxelSize%22%3A%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22%3A%5B108360%2C42086%2C3279%5D%7D%7D%2C%22zoomFactor%22%3A8%7D%2C%22perspectiveZoom%22%3A2230.6094%2C%22jsonStateServer%22%3A%22https%3A%2F%2Fglobalv1.flywire-daf.com%2Fnglstate%2Fpost%22%2C%22selectedLayer%22%3A%7B%22layer%22%3A%22Production-segmentation_with_graph%22%2C%22visible%22%3Atrue%7D%2C%22layout%22%3A%22xy-3d%22%7D"
# another way to do this, which long time R users may find more intuitive
as.character(fw_sc)
#> [1] "https://ngl.flywire.ai/#!%7B%22layers%22%3A%5B%7B%22source%22%3A%22precomputed%3A%2F%2Fgs%3A%2F%2Fmicrons-seunglab%2Fdrosophila_v0%2Falignment%2Fimage_rechunked%22%2C%22type%22%3A%22image%22%2C%22blend%22%3A%22default%22%2C%22shaderControls%22%3A%7B%7D%2C%22name%22%3A%22Production-image%22%7D%2C%7B%22source%22%3A%22graphene%3A%2F%2Fhttps%3A%2F%2Fprod.flywire-daf.com%2Fsegmentation%2Ftable%2Ffly_v31%22%2C%22type%22%3A%22segmentation_with_graph%22%2C%22segments%22%3A%5B%220%22%2C%22720575940626877799%22%5D%2C%22skeletonRendering%22%3A%7B%22mode2d%22%3A%22lines_and_points%22%2C%22mode3d%22%3A%22lines%22%7D%2C%22graphOperationMarker%22%3A%5B%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%5D%2C%22pathFinder%22%3A%7B%22color%22%3A%22%23ffff00%22%2C%22pathObject%22%3A%7B%22annotationPath%22%3A%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%22hasPath%22%3Afalse%7D%7D%2C%22name%22%3A%22Production-segmentation_with_graph%22%7D%5D%2C%22navigation%22%3A%7B%22pose%22%3A%7B%22position%22%3A%7B%22voxelSize%22%3A%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22%3A%5B108360%2C42086%2C3279%5D%7D%7D%2C%22zoomFactor%22%3A8%7D%2C%22perspectiveZoom%22%3A2230.6094%2C%22jsonStateServer%22%3A%22https%3A%2F%2Fglobalv1.flywire-daf.com%2Fnglstate%2Fpost%22%2C%22selectedLayer%22%3A%7B%22layer%22%3A%22Production-segmentation_with_graph%22%2C%22visible%22%3Atrue%7D%2C%22layout%22%3A%22xy-3d%22%7D"

if (FALSE) { # \dontrun{
# open in your default browser
browseURL(ngl_encode_url(fw_sc))
# ... or
browseURL(as.character(fw_sc))
} # }
# }

if (FALSE) { # \dontrun{
# copy JSON scene information from {} symbol at top right of neuroglancer
# now make a permanent URL for the scene
ngl_encode_url(clipr::read_clip())
} # }
```
