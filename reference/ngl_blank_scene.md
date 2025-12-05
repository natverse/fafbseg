# Return a blank neuroglancer scene based on a specified segmentation

defaults to the current segmentation defined by
[`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
when `release=NULL`

## Usage

``` r
ngl_blank_scene(release = NULL, return.url = FALSE)
```

## Arguments

- release:

  character vector specifying a released segmentation via a known short
  name or a sample neuroglancer URL.

- return.url:

  Whether to return a URL rather than a `ngscene` object.

## See also

[`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)

Other neuroglancer-urls:
[`flywire_ids()`](https://natverse.org/fafbseg/reference/flywire_ids.md),
[`flywire_scene()`](https://natverse.org/fafbseg/reference/flywire_scene.md),
[`ngl_decode_scene()`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md),
[`ngl_encode_url()`](https://natverse.org/fafbseg/reference/ngl_encode_url.md),
[`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md),
[`open_fafb_ngl()`](https://natverse.org/fafbseg/reference/open_fafb_ngl.md)

## Examples

``` r
# blank scene for current segmentation
ngl_blank_scene()
#> neuroglancer scene with 2 layers and 0 segments (of which 0 shown)
#>                                 name                    type archived visible
#> 1                   Production-image                   image    FALSE    TRUE
#> 2 Production-segmentation_with_graph segmentation_with_graph    FALSE    TRUE
#>   nsegs nhidden
#> 1     0       0
#> 2     0       0
#>                                                                        source
#> 1 precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked
#> 2          graphene://https://prod.flywire-daf.com/segmentation/table/fly_v31
# add a specific id
ngl_blank_scene()+"720575940623755722"
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
# a different segmentation
ngl_blank_scene("202004")
#> neuroglancer scene with 2 layers and 0 segments (of which 0 shown)
#>                 name         type archived visible nsegs nhidden
#> 1     fafb_v14_clahe        image    FALSE    TRUE     0       0
#> 2 fafb-ffn1-20200412 segmentation    FALSE    TRUE     0       0
#>                                                              source
#> 1 precomputed://gs://neuroglancer-fafb-data/fafb_v14/fafb_v14_clahe
#> 2                precomputed://gs://fafb-ffn1-20200412/segmentation
u=ngl_blank_scene("202004", return.url=TRUE)
if (FALSE) { # \dontrun{
u=ngl_blank_scene("sandbox")
browseURL(u)
} # }
```
