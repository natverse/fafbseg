# Construct Neuroglancer URL based on 3D location data

Construct Neuroglancer URL based on 3D location data

## Usage

``` r
open_fafb_ngl(
  x,
  s = rgl::select3d(),
  zoomFactor = 8,
  coords.only = FALSE,
  open = interactive() && !coords.only,
  sample = NULL,
  reference = NULL,
  sampleurl = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector OR any object compatible with
  [`xyzmatrix`](https://rdrr.io/pkg/nat/man/xyzmatrix.html) OR a CATMAID
  URL (see details)

- s:

  Optional selection function of the type returned by
  [`select3d`](https://dmurdoch.github.io/rgl/dev/reference/select3d.html)

- zoomFactor:

  The Neuroglancer zoomFactor (bigger means zoomed out)

- coords.only:

  Return raw coordinate string for pasting into Neuroglancer position
  widget (top left of screen)

- open:

  Whether or not to open the URL in a browser - this defaults to `TRUE`
  in interactive use.

- sample, reference:

  Template space of the input object `sample` and target (`reference`).
  See examples and `xform_brain` for details of how these are specified.

- sampleurl:

  A sample URL that defines your Neuroglancer dataset.

- ...:

  Additional arguments passed to
  [`fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  to control JSON parsing.

## Value

A character vector with a Neuroglancer URL or coordinate string
(invisibly when `open=TRUE`)

## Details

Neuroglancer scenes seem to be specified in a single URL that encodes a
json object defining layers to display, position etc. This function
works by taking a sample URL defining such a scene and then editing it
to point to a new 3D location / adjust zoom.

This package comes with 4 different default scene urls specified via
[`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
or
[`with_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md).
This is the easiest way to choose a particular segmentation. You can
also specify a different sample URL via the `sampleurl` argument; it
will be remembered for the rest of the R session. If you regularly use a
particular kind of scene URL, you can set `options(fafbseg.sampleurl)`
in your [`Rprofile`](https://rdrr.io/r/base/Startup.html) file.

## See also

Other neuroglancer-urls:
[`flywire_ids()`](https://natverse.org/fafbseg/reference/flywire_ids.md),
[`flywire_scene()`](https://natverse.org/fafbseg/reference/flywire_scene.md),
[`ngl_blank_scene()`](https://natverse.org/fafbseg/reference/ngl_blank_scene.md),
[`ngl_decode_scene()`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md),
[`ngl_encode_url()`](https://natverse.org/fafbseg/reference/ngl_encode_url.md),
[`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md)

## Examples

``` r
u=paste0("https://fafb.catmaid.virtualflybrain.org/?pid=2&zp=131280&",
"yp=170014.98879622458&xp=426584.81386896875&tool=navigator&sid0=2&s0=-1")

# translate URL but don't open browser
open_fafb_ngl(u, open=FALSE)
#> [1] "https://ngl.flywire.ai/#!%7B%22layers%22%3A%5B%7B%22source%22%3A%22precomputed%3A%2F%2Fgs%3A%2F%2Fmicrons-seunglab%2Fdrosophila_v0%2Falignment%2Fimage_rechunked%22%2C%22type%22%3A%22image%22%2C%22blend%22%3A%22default%22%2C%22shaderControls%22%3A%7B%7D%2C%22name%22%3A%22Production-image%22%7D%2C%7B%22source%22%3A%22graphene%3A%2F%2Fhttps%3A%2F%2Fprod.flywire-daf.com%2Fsegmentation%2Ftable%2Ffly_v31%22%2C%22type%22%3A%22segmentation_with_graph%22%2C%22segments%22%3A%5B%220%22%5D%2C%22skeletonRendering%22%3A%7B%22mode2d%22%3A%22lines_and_points%22%2C%22mode3d%22%3A%22lines%22%7D%2C%22graphOperationMarker%22%3A%5B%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%5D%2C%22pathFinder%22%3A%7B%22color%22%3A%22%23ffff00%22%2C%22pathObject%22%3A%7B%22annotationPath%22%3A%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%22hasPath%22%3Afalse%7D%7D%2C%22name%22%3A%22Production-segmentation_with_graph%22%7D%5D%2C%22navigation%22%3A%7B%22pose%22%3A%7B%22position%22%3A%7B%22voxelSize%22%3A%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22%3A%5B106646.2035%2C42503.7472%2C3282%5D%7D%7D%2C%22zoomFactor%22%3A8%7D%2C%22perspectiveZoom%22%3A2230.6094%2C%22jsonStateServer%22%3A%22https%3A%2F%2Fglobalv1.flywire-daf.com%2Fnglstate%2Fpost%22%2C%22selectedLayer%22%3A%7B%22layer%22%3A%22Production-segmentation_with_graph%22%2C%22visible%22%3Atrue%7D%2C%22layout%22%3A%22xy-3d%22%7D"

# produce an x,y,z string to paste into Neuroglancer
open_fafb_ngl(u, coords.only=TRUE)
#> [1] "106646.203467242,42503.7471990561,3282"

# translate URL converting from FAFB14 to FlyWire coordinates
# (only a small shift)
open_fafb_ngl(u, sample="FAFB14", reference="FlyWire", open=FALSE)
#> [1] "https://ngl.flywire.ai/#!%7B%22layers%22%3A%5B%7B%22source%22%3A%22precomputed%3A%2F%2Fgs%3A%2F%2Fmicrons-seunglab%2Fdrosophila_v0%2Falignment%2Fimage_rechunked%22%2C%22type%22%3A%22image%22%2C%22blend%22%3A%22default%22%2C%22shaderControls%22%3A%7B%7D%2C%22name%22%3A%22Production-image%22%7D%2C%7B%22source%22%3A%22graphene%3A%2F%2Fhttps%3A%2F%2Fprod.flywire-daf.com%2Fsegmentation%2Ftable%2Ffly_v31%22%2C%22type%22%3A%22segmentation_with_graph%22%2C%22segments%22%3A%5B%220%22%5D%2C%22skeletonRendering%22%3A%7B%22mode2d%22%3A%22lines_and_points%22%2C%22mode3d%22%3A%22lines%22%7D%2C%22graphOperationMarker%22%3A%5B%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%5D%2C%22pathFinder%22%3A%7B%22color%22%3A%22%23ffff00%22%2C%22pathObject%22%3A%7B%22annotationPath%22%3A%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%22hasPath%22%3Afalse%7D%7D%2C%22name%22%3A%22Production-segmentation_with_graph%22%7D%5D%2C%22navigation%22%3A%7B%22pose%22%3A%7B%22position%22%3A%7B%22voxelSize%22%3A%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22%3A%5B106560.9535%2C42529.9972%2C3282%5D%7D%7D%2C%22zoomFactor%22%3A8%7D%2C%22perspectiveZoom%22%3A2230.6094%2C%22jsonStateServer%22%3A%22https%3A%2F%2Fglobalv1.flywire-daf.com%2Fnglstate%2Fpost%22%2C%22selectedLayer%22%3A%7B%22layer%22%3A%22Production-segmentation_with_graph%22%2C%22visible%22%3Atrue%7D%2C%22layout%22%3A%22xy-3d%22%7D"
if (FALSE) { # \dontrun{
# copy CATMAID URL from clipboard and Neuroglancer coords to clipboard
clipr::write_clip(open_fafb_ngl(clipr::read_clip(), coords.only=TRUE))

# Open a location in MB peduncle with current preferred segmentation
open_fafb_ngl(c(433440, 168344, 131200))

# choose a particular segmentation (Google FAFB)
with_segmentation("20190805", open_fafb_ngl(c(433440, 168344, 131200),
zoomFactor=2))

# or FlyWire
with_segmentation("flywire", open_fafb_ngl(c(433440, 168344, 131200)))

# ... and translate FAFB14 to FlyWire coordinates
with_segmentation("flywire", open_fafb_ngl(c(433440, 168344, 131200),
sample="FAFB14", reference="FlyWire", zoomFactor=2))

# open a CATMAID URL in Neuroglancer
open_fafb_ngl(u)

# Set an existing scene URL (pointing to any old location) to act as
# the template for open_fafb_ngl
# nb the package sets one for you on startup if you haven't set yourself
options(fafbseg.sampleurl="https://<neuroglancerlurl>")
# Edit your R profile if you want to set a different default
usethis::edit_r_profile()
} # }
```
