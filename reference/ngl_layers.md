# Extract and manipulate layers in a neuroglancer scene

`ngl_layers` extract the neuroglancer layers with convenience options
for selecting layers by characteristics such as visibility, type etc.

`ngl_layers<-` sets the layers element of a
[`ngscene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md)
object, taking care of name/class details.

`+.ngscene` adds segments or layers to a neuroglancer scene

`-.ngscene` removes segments or whole layers from a neuroglancer scene.
It does not complain if the segment is not present.

## Usage

``` r
ngl_layers(x, subset = NULL)

ngl_layers(x) <- value

# S3 method for class 'ngscene'
x + y

# S3 method for class 'ngscene'
x - y
```

## Arguments

- x:

  a neuroglancer scene object (see
  [`ngscene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md))
  or an existing `nglayers` object (which you probably want to subset).

- subset:

  an expression (evaluated in the style of subset.dataframe) which
  defined

- value:

  a list specifying one or more neuroglancer layers. This will usually
  come from a json fragment or another parsed neuroglancer scene. See
  examples.

- y:

  Segments or layers to add or remove from a neuroglancer scene.
  Segments are provided as character vectors or by applying
  [`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  to a more complex object. Layers to remove should be the layer name.
  Layers to add should be in the form of an R list returned by ng_layers
  or a JSON fragment copied from neuroglancer.

## Value

A list of layers with additional class `nglayers`

## Using + and -

There are shortcut methods that allow you to add or subtract segments or
layers from neuroglancer scenes. These are designed for convenience in
interactive use, but may be a bit fragile for unusual inputs.

## See also

[`ngl_decode_scene`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md),
`ngl_layers`,
[`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md),
[`ngl_encode_url`](https://natverse.org/fafbseg/reference/ngl_encode_url.md)

## Examples

``` r
# \donttest{
u="https://ngl.flywire.ai/?json_url=https://globalv1.flywire-daf.com/nglstate/5409525645443072"
sc=ngl_decode_scene(u)
sc
#> neuroglancer scene with 2 layers and 2 segments (of which 2 shown)
#>                                 name                    type archived visible
#> 1                   Production-image                   image    FALSE    TRUE
#> 2 Production-segmentation_with_graph segmentation_with_graph    FALSE    TRUE
#>   nsegs nhidden
#> 1     0       0
#> 2     2       0
#>                                                                        source
#> 1 precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked
#> 2          graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31
names(ngl_layers(sc))
#> [1] "Production-image"                   "Production-segmentation_with_graph"
str(ngl_layers(sc))
#> List of 2
#>  $ Production-image                  :List of 5
#>   ..$ source        : chr "precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked"
#>   ..$ type          : chr "image"
#>   ..$ blend         : chr "default"
#>   ..$ shaderControls: Named list()
#>   ..$ name          : chr "Production-image"
#>  $ Production-segmentation_with_graph:List of 7
#>   ..$ source              : chr "graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31"
#>   ..$ type                : chr "segmentation_with_graph"
#>   ..$ segments            : chr [1:2] "720575940621039145" "720575940626877799"
#>   ..$ skeletonRendering   :List of 2
#>   .. ..$ mode2d: chr "lines_and_points"
#>   .. ..$ mode3d: chr "lines"
#>   ..$ graphOperationMarker:List of 2
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   ..$ pathFinder          :List of 2
#>   .. ..$ color     : chr "#ffff00"
#>   .. ..$ pathObject:List of 2
#>   .. .. ..$ annotationPath:List of 2
#>   .. .. .. ..$ annotations: list()
#>   .. .. .. ..$ tags       : list()
#>   .. .. ..$ hasPath       : logi FALSE
#>   ..$ name                : chr "Production-segmentation_with_graph"
#>  - attr(*, "class")= chr [1:2] "nglayers" "list"

str(ngl_layers(sc, nsegs>0))
#> List of 1
#>  $ Production-segmentation_with_graph:List of 7
#>   ..$ source              : chr "graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31"
#>   ..$ type                : chr "segmentation_with_graph"
#>   ..$ segments            : chr [1:2] "720575940621039145" "720575940626877799"
#>   ..$ skeletonRendering   :List of 2
#>   .. ..$ mode2d: chr "lines_and_points"
#>   .. ..$ mode3d: chr "lines"
#>   ..$ graphOperationMarker:List of 2
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   ..$ pathFinder          :List of 2
#>   .. ..$ color     : chr "#ffff00"
#>   .. ..$ pathObject:List of 2
#>   .. .. ..$ annotationPath:List of 2
#>   .. .. .. ..$ annotations: list()
#>   .. .. .. ..$ tags       : list()
#>   .. .. ..$ hasPath       : logi FALSE
#>   ..$ name                : chr "Production-segmentation_with_graph"
#>  - attr(*, "class")= chr [1:2] "nglayers" "list"
str(ngl_layers(sc, visible==TRUE))
#> List of 2
#>  $ Production-image                  :List of 5
#>   ..$ source        : chr "precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked"
#>   ..$ type          : chr "image"
#>   ..$ blend         : chr "default"
#>   ..$ shaderControls: Named list()
#>   ..$ name          : chr "Production-image"
#>  $ Production-segmentation_with_graph:List of 7
#>   ..$ source              : chr "graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31"
#>   ..$ type                : chr "segmentation_with_graph"
#>   ..$ segments            : chr [1:2] "720575940621039145" "720575940626877799"
#>   ..$ skeletonRendering   :List of 2
#>   .. ..$ mode2d: chr "lines_and_points"
#>   .. ..$ mode3d: chr "lines"
#>   ..$ graphOperationMarker:List of 2
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   ..$ pathFinder          :List of 2
#>   .. ..$ color     : chr "#ffff00"
#>   .. ..$ pathObject:List of 2
#>   .. .. ..$ annotationPath:List of 2
#>   .. .. .. ..$ annotations: list()
#>   .. .. .. ..$ tags       : list()
#>   .. .. ..$ hasPath       : logi FALSE
#>   ..$ name                : chr "Production-segmentation_with_graph"
#>  - attr(*, "class")= chr [1:2] "nglayers" "list"
str(ngl_layers(sc, !visible))
#>  Named list()
#>  - attr(*, "class")= chr [1:2] "nglayers" "list"
# flywire segmentation
str(ngl_layers(sc, type=="segmentation_with_graph"))
#> List of 1
#>  $ Production-segmentation_with_graph:List of 7
#>   ..$ source              : chr "graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31"
#>   ..$ type                : chr "segmentation_with_graph"
#>   ..$ segments            : chr [1:2] "720575940621039145" "720575940626877799"
#>   ..$ skeletonRendering   :List of 2
#>   .. ..$ mode2d: chr "lines_and_points"
#>   .. ..$ mode3d: chr "lines"
#>   ..$ graphOperationMarker:List of 2
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   ..$ pathFinder          :List of 2
#>   .. ..$ color     : chr "#ffff00"
#>   .. ..$ pathObject:List of 2
#>   .. .. ..$ annotationPath:List of 2
#>   .. .. .. ..$ annotations: list()
#>   .. .. .. ..$ tags       : list()
#>   .. .. ..$ hasPath       : logi FALSE
#>   ..$ name                : chr "Production-segmentation_with_graph"
#>  - attr(*, "class")= chr [1:2] "nglayers" "list"
# image or segmentation
str(ngl_layers(sc, type %in% c("image", "segmentation_with_graph")))
#> List of 2
#>  $ Production-image                  :List of 5
#>   ..$ source        : chr "precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked"
#>   ..$ type          : chr "image"
#>   ..$ blend         : chr "default"
#>   ..$ shaderControls: Named list()
#>   ..$ name          : chr "Production-image"
#>  $ Production-segmentation_with_graph:List of 7
#>   ..$ source              : chr "graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31"
#>   ..$ type                : chr "segmentation_with_graph"
#>   ..$ segments            : chr [1:2] "720575940621039145" "720575940626877799"
#>   ..$ skeletonRendering   :List of 2
#>   .. ..$ mode2d: chr "lines_and_points"
#>   .. ..$ mode3d: chr "lines"
#>   ..$ graphOperationMarker:List of 2
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   .. ..$ :List of 2
#>   .. .. ..$ annotations: list()
#>   .. .. ..$ tags       : list()
#>   ..$ pathFinder          :List of 2
#>   .. ..$ color     : chr "#ffff00"
#>   .. ..$ pathObject:List of 2
#>   .. .. ..$ annotationPath:List of 2
#>   .. .. .. ..$ annotations: list()
#>   .. .. .. ..$ tags       : list()
#>   .. .. ..$ hasPath       : logi FALSE
#>   ..$ name                : chr "Production-segmentation_with_graph"
#>  - attr(*, "class")= chr [1:2] "nglayers" "list"
# }
# get a sample flywire neuroglancer scene
sc=ngl_decode_scene(system.file("flywire-annotations.json" ,
  package = 'fafbseg'))
sc
#> neuroglancer scene with 4 layers and 43 segments (of which 1 shown)
#>                                 name                    type archived visible
#> 1                   Production-image                   image    FALSE    TRUE
#> 2 Production-segmentation_with_graph segmentation_with_graph    FALSE    TRUE
#> 3                         annotation              annotation    FALSE    TRUE
#> 4                     jfrc_mesh_test            segmentation    FALSE   FALSE
#>   nsegs nhidden
#> 1     0       0
#> 2     1      42
#> 3     0       0
#> 4     1       1
#>                                                                        source
#> 1 precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked
#> 2          graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31
#> 3                                                                        <NA>
#> 4             precomputed://https://spine.itanna.io/files/eric/jfrc_mesh_test
# save a copy
sc.orig <- sc
# remove a layer
ngl_layers(sc)=ngl_layers(sc)[-3]
# or using convenient - notation
sc.noann <- sc.orig - "annotation"

# reverse layer order
ngl_layers(sc)=ngl_layers(sc)[2:1]

# keep visible only
ngl_layers(sc) <- ngl_layers(sc, visible)
# visible + multiple segments
ngl_layers(sc) <- ngl_layers(sc, visible & nsegs>0)
# flywire segmentation
ngl_layers(sc) <- ngl_layers(sc, type=="segmentation_with_graph")

# combine layers using + convenience method
sc.noann + ngl_layers(sc.orig)['annotation']
#> neuroglancer scene with 4 layers and 43 segments (of which 1 shown)
#>                                 name                    type archived visible
#> 1                   Production-image                   image    FALSE    TRUE
#> 2 Production-segmentation_with_graph segmentation_with_graph    FALSE    TRUE
#> 3                     jfrc_mesh_test            segmentation    FALSE   FALSE
#> 4                         annotation              annotation    FALSE    TRUE
#>   nsegs nhidden
#> 1     0       0
#> 2     1      42
#> 3     1       1
#> 4     0       0
#>                                                                        source
#> 1 precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked
#> 2          graphene://https://prodv1.flywire-daf.com/segmentation/1.0/fly_v31
#> 3             precomputed://https://spine.itanna.io/files/eric/jfrc_mesh_test
#> 4                                                                        <NA>
if (FALSE) { # \dontrun{
# combine layers from two scenes
ngl_layers(sc) <- c(ngl_layers(sc), ngl_layers(sc2))
ngl_layers(sc) <- c(ngl_layers(sc)[-(3:4)], ngl_layers(sc2)[3:4])
ngl_layers(sc) <- c(ngl_layers(sc), ngl_layers(sc2)[-1])
ngl_layers(sc) <- c(ngl_layers(sc), ngl_layers(sc2)['annotation'])
sc

# another way to add a single scene
ngl_layers(sc)[[4]] <- ngl_layers(sc2)[[4]]

# add a new layer to a scene by parsing some JSON from the clipboard
# note the double brackets are essential here
ngl_layers(sc)[['jfrc_mesh']] <- jsonlite::fromJSON(clipr::read_clip())
} # }
```
