# Decode and manipulate Neuroglancer scenes (from URLs or JSON blocks)

`ngl_decode_scene` takes a Neuroglancer scene from your web browser and
turns it into an R `list` object that can be programmatically
manipulated e.g. to add/remove segments. Manipulate these scenes with
[`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md),
[`ngl_layers`](https://natverse.org/fafbseg/reference/ngl_layers.md).
See
[`ngl_encode_url`](https://natverse.org/fafbseg/reference/ngl_encode_url.md)
to turn a scene back into a URL to open in your browser.

## Usage

``` r
ngl_decode_scene(
  x,
  return.json = FALSE,
  simplifyVector = TRUE,
  simplifyDataFrame = FALSE,
  ...
)
```

## Arguments

- x:

  Character vector containing single Neuroglancer URL or a json block

- return.json:

  When `TRUE` extracts the JSON block in a URL does not parse it to an R
  list

- simplifyVector:

  coerce JSON arrays containing only primitives into an atomic vector

- simplifyDataFrame:

  coerce JSON arrays containing only records (JSON objects) into a data
  frame

- ...:

  additional arguments passed to
  [`jsonlite::fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)

## Value

An R list with additional class `ngscene` describing the scene, or, when
`return.json=TRUE`, a character vector.

## See also

[`URLdecode`](https://rdrr.io/r/utils/URLencode.html),
[`fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)

Other neuroglancer-urls:
[`flywire_ids()`](https://natverse.org/fafbseg/reference/flywire_ids.md),
[`flywire_scene()`](https://natverse.org/fafbseg/reference/flywire_scene.md),
[`ngl_blank_scene()`](https://natverse.org/fafbseg/reference/ngl_blank_scene.md),
[`ngl_encode_url()`](https://natverse.org/fafbseg/reference/ngl_encode_url.md),
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
fw_sc
#> neuroglancer scene with 2 layers and 1 segments (of which 1 shown)
#>                                 name                    type archived visible
#> 1                   Production-image                   image    FALSE    TRUE
#> 2 Production-segmentation_with_graph segmentation_with_graph    FALSE    TRUE
#>   nsegs nhidden
#> 1     0       0
#> 2     1       0
#>                                                                        source
#> 1 precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked
#> 2          graphene://https://prod.flywire-daf.com/segmentation/table/fly_v31
# add two segments
fw_sc=fw_sc+c("720575940621039145", "720575940626877799")
ngl_segments(fw_sc)
#> [1] "0"                  "720575940621039145" "720575940626877799"
# remove that 0 segment
fw_sc=fw_sc-0
ngl_segments(fw_sc)
#> [1] "720575940621039145" "720575940626877799"
# repeated segments are ignored i.e. no duplicates
ngl_segments(fw_sc+"720575940621039145")
#> [1] "720575940621039145" "720575940626877799"
# convert back to a URL, nb this depends on choose_segmentation
ngl_encode_url(fw_sc)
#> [1] "https://ngl.flywire.ai/#!%7B%22layers%22%3A%5B%7B%22source%22%3A%22precomputed%3A%2F%2Fgs%3A%2F%2Fmicrons-seunglab%2Fdrosophila_v0%2Falignment%2Fimage_rechunked%22%2C%22type%22%3A%22image%22%2C%22blend%22%3A%22default%22%2C%22shaderControls%22%3A%7B%7D%2C%22name%22%3A%22Production-image%22%7D%2C%7B%22source%22%3A%22graphene%3A%2F%2Fhttps%3A%2F%2Fprod.flywire-daf.com%2Fsegmentation%2Ftable%2Ffly_v31%22%2C%22type%22%3A%22segmentation_with_graph%22%2C%22segments%22%3A%5B%22720575940621039145%22%2C%22720575940626877799%22%5D%2C%22skeletonRendering%22%3A%7B%22mode2d%22%3A%22lines_and_points%22%2C%22mode3d%22%3A%22lines%22%7D%2C%22graphOperationMarker%22%3A%5B%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%5D%2C%22pathFinder%22%3A%7B%22color%22%3A%22%23ffff00%22%2C%22pathObject%22%3A%7B%22annotationPath%22%3A%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%22hasPath%22%3Afalse%7D%7D%2C%22name%22%3A%22Production-segmentation_with_graph%22%7D%5D%2C%22navigation%22%3A%7B%22pose%22%3A%7B%22position%22%3A%7B%22voxelSize%22%3A%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22%3A%5B108360%2C42086%2C3279%5D%7D%7D%2C%22zoomFactor%22%3A8%7D%2C%22perspectiveZoom%22%3A2230.6094%2C%22jsonStateServer%22%3A%22https%3A%2F%2Fglobalv1.flywire-daf.com%2Fnglstate%2Fpost%22%2C%22selectedLayer%22%3A%7B%22layer%22%3A%22Production-segmentation_with_graph%22%2C%22visible%22%3Atrue%7D%2C%22layout%22%3A%22xy-3d%22%7D"

if (FALSE) { # \dontrun{
# open in your default browser
browseURL(ngl_encode_url(fw_sc))
} # }
# }

if (FALSE) { # \dontrun{
ngl_decode_scene("<someneuroglancerurl>")

# decode scene from URL currently on clipboard
scene=ngl_decode_scene(clipr::read_clip())

# open a Neuroglancer URL in CATMAID
ngu="<someurl>"
library(elmr)
open_fafb(ngl_decode_scene(ngu))
# Or store the URL rather than opening it
cmu=open_fafb(ngl_decode_scene(ngu), open=FALSE)
} # }
```
