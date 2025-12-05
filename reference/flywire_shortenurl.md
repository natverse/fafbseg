# Shorten or expand neuroglancer URLs

`flywire_shortenurl` makes short URLs from long URLs or
[`ngscene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md)
objects that you may have constructed in R.

`flywire_expandurl` expands shortened URLs into a full neuroglancer JSON
scene specification. If the link references a specific version of
neuroglancer on a specific host URL then that will be used as the base
of the expanded URL. This is nearly always the case, but should this
ever not be so, then if the active segmentation
([`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md))
is a flywire segmentation then that is used to define the initial part
of the output URL. Failing this, the `flywire31` segmentation is used.

`flywire_expandurl` will also expand tinyurl.com URLs as well as those
referencing a json fragment on a google cloud bucket (such as the flyem
link shortener). If a tinyurl.com URL maps to a short URL referencing a
json fragment, then they will successively be expanded unless
`follow=FALSE`.

Finally, if the URL is actually already expanded, then this will be
returned unmodified. This is a change in behaviour as of May 2024
(previously an error was thrown).

## Usage

``` r
flywire_shortenurl(x, include_base = TRUE, baseurl = NULL, cache = TRUE, ...)

flywire_expandurl(x, json.only = FALSE, cache = TRUE, follow = TRUE, ...)
```

## Arguments

- x:

  One or more neuroglancer URLs or (for flywire_expandurl)
  [`ngscene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md)
  parsed scene description.

- include_base:

  Whether to return a full URL that will open a neuroglancer session
  (the default) or only the component that defines the scene (which
  would display JSON in your browser).

- baseurl:

  Optional URL defining the neuroglancer browser to use with shortened
  URLs.

- cache:

  Whether to cache any calls to the flywire state server shortening or
  expanding URLs. Default is `TRUE`. NB this cache is only active for
  the current session.

- ...:

  Additional arguments passed to `pbsapply` (when multiple URLs to
  process) and then to
  [`ngl_encode_url`](https://natverse.org/fafbseg/reference/ngl_encode_url.md)
  (when generating a short URL for an `ngscene` list object) *or* to
  `flywire_fetch` when using `flywire_expandurl`.

- json.only:

  Only return the JSON fragment rather than the neuroglancer URL.
  Defaults to `FALSE`.

- follow:

  Whether to follow short URLs that specify other short URLs (see
  details). Defaults to `TRUE`.

## Value

A character vector containing one or more URLs.

## Examples

``` r
if (FALSE) { # \dontrun{
sc=ngl_blank_scene()
short=flywire_shortenurl(sc)
long=flywire_expandurl(short)
} # }
# \donttest{
flywire_expandurl("https://globalv1.flywire-daf.com/nglstate/5747205470158848")
#> [1] "https://globalv1.flywire-daf.com/#!%7B%22layers%22%3A%5B%7B%22source%22%3A%22precomputed%3A%2F%2Fgs%3A%2F%2Fmicrons-seunglab%2Fdrosophila_v0%2Falignment%2Fimage_rechunked%22%2C%22type%22%3A%22image%22%2C%22blend%22%3A%22default%22%2C%22shaderControls%22%3A%7B%7D%2C%22name%22%3A%22Production-image%22%7D%2C%7B%22tab%22%3A%22graph%22%2C%22source%22%3A%22graphene%3A%2F%2Fhttps%3A%2F%2Fprodv1.flywire-daf.com%2Fsegmentation%2F1.0%2Ffly_v31%22%2C%22type%22%3A%22segmentation_with_graph%22%2C%22selectedAlpha%22%3A0.62%2C%22colorSeed%22%3A1358768360%2C%22segments%22%3A%5B%22720575940619527173%22%2C%22720575940628130268%22%2C%22720575940630484179%22%5D%2C%22hiddenSegments%22%3A%5B%22720575940634222734%22%2C%22720575940636364870%22%5D%2C%22skeletonRendering%22%3A%7B%22mode2d%22%3A%22lines_and_points%22%2C%22mode3d%22%3A%22lines%22%7D%2C%22graphOperationMarker%22%3A%5B%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%5D%2C%22pathFinder%22%3A%7B%22color%22%3A%22%23ffff00%22%2C%22pathObject%22%3A%7B%22annotationPath%22%3A%7B%22annotations%22%3A%5B%5D%2C%22tags%22%3A%5B%5D%7D%2C%22hasPath%22%3Afalse%7D%7D%2C%22name%22%3A%22Production-segmentation_with_graph%22%7D%5D%2C%22navigation%22%3A%7B%22pose%22%3A%7B%22position%22%3A%7B%22voxelSize%22%3A%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22%3A%5B115198%2C55453%2C1537%5D%7D%7D%2C%22zoomFactor%22%3A4.145162302892657%7D%2C%22perspectiveOrientation%22%3A%5B-0.22566604614257812%2C-0.034010399132966995%2C-0.07959923148155212%2C0.97035151720047%5D%2C%22perspectiveZoom%22%3A4888.123757966904%2C%22showSlices%22%3Afalse%2C%22jsonStateServer%22%3A%22https%3A%2F%2Fglobalv1.flywire-daf.com%2Fnglstate%2Fpost%22%2C%22selectedLayer%22%3A%7B%22layer%22%3A%22Production-segmentation_with_graph%22%2C%22visible%22%3Atrue%7D%2C%22layout%22%3A%22xy-3d%22%7D"
flywire_expandurl("https://tinyurl.com/rmr58jpn")
#> [1] "https://neuroglancer-demo.appspot.com/#!%7B%22dimensions%22:%7B%22x%22:%5B1.6e-8%2C%22m%22%5D%2C%22y%22:%5B1.6e-8%2C%22m%22%5D%2C%22z%22:%5B4e-8%2C%22m%22%5D%7D%2C%22position%22:%5B35783.34765625%2C10234.412109375%2C3672.360107421875%5D%2C%22crossSectionScale%22:1.2071435133745394%2C%22projectionOrientation%22:%5B0.3210762143135071%2C-0.019618114456534386%2C-0.03835516422986984%2C-0.9460729956626892%5D%2C%22projectionScale%22:17861.319479912258%2C%22layers%22:%5B%7B%22type%22:%22image%22%2C%22source%22:%22precomputed://gs://flywire_em/aligned/v1%22%2C%22tab%22:%22source%22%2C%22name%22:%22v1%22%7D%2C%7B%22type%22:%22segmentation%22%2C%22source%22:%22graphene://middleauth+https://prod.flywire-daf.com/segmentation/1.0/fly_v31%22%2C%22tab%22:%22segments%22%2C%22segments%22:%5B%22720575940626927366%22%2C%22720575940632115923%22%5D%2C%22segmentQuery%22:%22720575940626927366%20720575940632115923%22%2C%22colorSeed%22:3185436646%2C%22name%22:%22fly_v31%22%7D%2C%7B%22type%22:%22segmentation%22%2C%22source%22:%7B%22url%22:%22graphene://middleauth+https://prod.flywire-daf.com/segmentation/1.0/fly_v31%22%2C%22transform%22:%7B%22matrix%22:%5B%5B-0.9923%2C-0.0451%2C0.0652%2C65672.5875%5D%2C%5B-0.0765%2C0.9961%2C0.0048%2C2454.31625%5D%2C%5B0.167%2C0.0059%2C0.9915%2C-2227.49425%5D%5D%2C%22outputDimensions%22:%7B%22x%22:%5B1.6e-8%2C%22m%22%5D%2C%22y%22:%5B1.6e-8%2C%22m%22%5D%2C%22z%22:%5B4e-8%2C%22m%22%5D%7D%7D%2C%22subsources%22:%7B%22default%22:true%2C%22graph%22:true%2C%22bounds%22:true%2C%22mesh%22:true%7D%2C%22enableDefaultSubsources%22:false%7D%2C%22tab%22:%22source%22%2C%22segments%22:%5B%22720575940626927366%22%5D%2C%22segmentQuery%22:%22720575940626927366%20720575940632115923%22%2C%22colorSeed%22:626504786%2C%22name%22:%22fly_v31_mirror%22%2C%22archived%22:true%7D%2C%7B%22type%22:%22segmentation%22%2C%22source%22:%22precomputed://https://spine.itanna.io/files/eric/jfrc_mesh_test%22%2C%22tab%22:%22source%22%2C%22selectedAlpha%22:0.55%2C%22objectAlpha%22:0.09%2C%22segments%22:%5B%221%22%5D%2C%22colorSeed%22:2225652644%2C%22segmentColors%22:%7B%221%22:%22#cacdd8%22%7D%2C%22name%22:%22jfrc_mesh_test1%22%2C%22visible%22:false%7D%5D%2C%22showAxisLines%22:false%2C%22selectedLayer%22:%7B%22size%22:646%2C%22visible%22:true%2C%22layer%22:%22fly_v31_mirror%22%7D%2C%22layout%22:%7B%22type%22:%223d%22%2C%22orthographicProjection%22:true%7D%2C%22selection%22:%7B%22size%22:646%2C%22visible%22:false%7D%2C%22layerListPanel%22:%7B%22visible%22:true%7D%7D"
# }
if (FALSE) { # \dontrun{
flywire_expandurl("https://tinyurl.com/flywirehb2")
} # }
```
