# Add colours to the neuroglancer scene

Add colours to the neuroglancer scene

## Usage

``` r
ngl_add_colours(x, colours, layer = NULL)
```

## Arguments

- x:

  neuroglancer scene in any form acceptable to
  [`ngl_decode_scene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md)
  (including as a URL)

- colours:

  A dataframe with two columns, where the first is the id and the second
  is the colour, OR a character vector of colours named by the ids or
  one colour which would be added to all the displayed neurons. See
  [`col2rgb`](https://rdrr.io/r/grDevices/col2rgb.html) for additional
  details of how col can be specified.

- layer:

  Optional character vector specifying the layer to colour. When
  `lyaer=NULL` (the default) will choose a layer of type
  segmentation_with_graph if one exists.

## Value

A neuroglancer scene object (see
[`ngl_decode_scene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md))

## Examples

``` r
fw_url=with_segmentation('flywire31', getOption('fafbseg.sampleurl'))
ngl_add_colours(fw_url, colours=c("720575940614404544"="red"))
#> neuroglancer scene with 2 layers and 2 segments (of which 2 shown)
#>                                 name                    type archived visible
#> 1                   Production-image                   image    FALSE    TRUE
#> 2 Production-segmentation_with_graph segmentation_with_graph    FALSE    TRUE
#>   nsegs nhidden
#> 1     0       0
#> 2     2       0
#>                                                                        source
#> 1 precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked
#> 2          graphene://https://prod.flywire-daf.com/segmentation/table/fly_v31

if (FALSE) { # \dontrun{
# colour all neurons in the URL on the clipboard red.
# Then convert back to URL and open in default browser
browseURL(as.character(ngl_add_colours(clipr::read_clip(), col="red")))

# Let's colour neurons from these 3 scenes in red, green and blue
u1="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5695474417795072"
u2="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5198787572137984"
u3="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5673953041317888"
# sequentially build up a data.frame with the colour information
# note that col will be recycled to the same length as the number of segments
colourdf=data.frame(ids=ngl_segments(u1), col='red')
colourdf=rbind(colourdf, data.frame(ids=ngl_segments(u2), col='green'))
colourdf=rbind(colourdf, data.frame(ids=ngl_segments(u3), col='blue'))
# apply that to the first URL
sc=ngl_add_colours(u1, colourdf)
browseURL(as.character(sc))
} # }
```
