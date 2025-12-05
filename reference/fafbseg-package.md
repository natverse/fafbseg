# fafbseg: Support Functions for Analysis of FAFB EM Segmentation

Functions that read some of the raw data formats produced by Google
electron microscopy segmentation tools as displayed by 'neuroglancer'
and then allow basic 3D visualisation and analysis.

## Package Options

- `fafbseg.sampleurl` optionally set to a sample Neuroglancer URL that
  will modified to point to arbitrary locations by
  [`open_fafb_ngl`](https://natverse.org/fafbseg/reference/open_fafb_ngl.md).

- `fafbseg.cave.datastack_name` optionally set to the `datastack_name`
  used by the `CAVE` system. Expert use only. Normally
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  looks after this. See
  [`flywire_cave_query`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)
  for further details.

- `fafbseg.condaenv` the python environment to use with miniconda.
  Defaults to `"r-reticulate"`, which is the environment set up by
  [`reticulate::install_miniconda`](https://rstudio.github.io/reticulate/reference/install_miniconda.html).
  You can set this to something different if you would like to keep a
  separate miniconda virtual environment just for fafbseg (expert use
  only).

- `fafbseg.sqlitepath` optional to set the location of SQLite tables
  used by
  [`flywire_partners`](https://natverse.org/fafbseg/reference/flywire_partners.md)
  and friends.

- `fafbseg.cachedir` The location for disk caches for functions
  including
  [`flywire_leaves`](https://natverse.org/fafbseg/reference/flywire_leaves.md)
  If unset on package load, will be set to an appropriate user folder
  using
  `rappdirs::`[`user_data_dir`](https://rappdirs.r-lib.org/reference/user_data_dir.html).

- `fafbseg.flcachesize` The maximum cache size in bytes. Defaults to
  `1.5 * 1024^3` when unset. See
  [`flywire_leaves`](https://natverse.org/fafbseg/reference/flywire_leaves.md)
  for details.

- `fafbseg.flywire_roots.chunksize` this will default to processing
  100,000 rootids at a time. Set this smaller if the queries time out /
  give "Request Entity Too Large" errors. Larger settings are unlikely
  to have much of a speed impact. See
  [`flywire_rootid`](https://natverse.org/fafbseg/reference/flywire_rootid.md)
  for details.

- `fafbseg.skelziproot` set to the location of a folder containing the
  zipped versions of the skeletonised segmentations. This will be used
  by
  [`read_segments`](https://natverse.org/fafbseg/reference/read_segments.md),
  [`read_topn`](https://natverse.org/fafbseg/reference/read_topn.md)
  etc.

- `fafbseg.skeletonuri` a brainmaps URI specifying a remote source used
  by
  [`read.neurons.brainmaps`](https://natverse.org/fafbseg/reference/read.neuron.brainmaps.md)
  and
  [`brainmaps_skeleton`](https://natverse.org/fafbseg/reference/brainmaps_skeleton.md)
  to read neuronal skeletons.

- `fafbseg.brainmaps_xyz2id.chunksize` this will default to querying
  4000 vertices at a time. Set this smaller if the queries time out or
  larger to speed things up. See
  [`brainmaps_xyz2id`](https://natverse.org/fafbseg/reference/brainmaps_xyz2id.md)
  for details.

## See also

Useful links:

- <https://github.com/natverse/fafbseg>

- Report bugs at <https://github.com/natverse/fafbseg/issues>

## Author

**Maintainer**: Gregory Jefferis <jefferis@gmail.com>
([ORCID](https://orcid.org/0000-0002-0587-9355))

Other contributors:

- Sridhar Jagannathan <j.sridharrajan@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-2078-1145)) \[contributor\]

- Alexander Bates <alexander.shakeel.bates@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-1195-0445)) \[contributor\]

## Examples

``` r
options()[grep("^fafbseg\\.", names(options()))]
#> $fafbseg.cachedir
#> [1] "/home/runner/.local/share/R/fafbseg"
#> 
#> $fafbseg.cave.datastack_name
#> [1] "flywire_fafb_production"
#> 
#> $fafbseg.cloudvolume.url
#> [1] "graphene://https://prod.flywire-daf.com/segmentation/table/fly_v31"
#> 
#> $fafbseg.condaenv
#> [1] "r-reticulate"
#> 
#> $fafbseg.sampleurl
#> [1] "https://ngl.flywire.ai/#!%7B%22layers%22:%5B%7B%22source%22:%22precomputed://gs://microns-seunglab/drosophila_v0/alignment/image_rechunked%22%2C%22type%22:%22image%22%2C%22blend%22:%22default%22%2C%22shaderControls%22:%7B%7D%2C%22name%22:%22Production-image%22%7D%2C%7B%22source%22:%22graphene://https://prod.flywire-daf.com/segmentation/table/fly_v31%22%2C%22type%22:%22segmentation_with_graph%22%2C%22segments%22:%5B%220%22%5D%2C%22skeletonRendering%22:%7B%22mode2d%22:%22lines_and_points%22%2C%22mode3d%22:%22lines%22%7D%2C%22graphOperationMarker%22:%5B%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%2C%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%5D%2C%22pathFinder%22:%7B%22color%22:%22#ffff00%22%2C%22pathObject%22:%7B%22annotationPath%22:%7B%22annotations%22:%5B%5D%2C%22tags%22:%5B%5D%7D%2C%22hasPath%22:false%7D%7D%2C%22name%22:%22Production-segmentation_with_graph%22%7D%5D%2C%22navigation%22:%7B%22pose%22:%7B%22position%22:%7B%22voxelSize%22:%5B4%2C4%2C40%5D%2C%22voxelCoordinates%22:%5B108360%2C42086%2C3279%5D%7D%7D%2C%22zoomFactor%22:8%7D%2C%22perspectiveZoom%22:2230.6094286986126%2C%22jsonStateServer%22:%22https://globalv1.flywire-daf.com/nglstate/post%22%2C%22selectedLayer%22:%7B%22layer%22:%22Production-segmentation_with_graph%22%2C%22visible%22:true%7D%2C%22layout%22:%22xy-3d%22%7D"
#> 
#> $fafbseg.sqlitepath
#> [1] "~/projects/JanFunke/"
#> 
```
