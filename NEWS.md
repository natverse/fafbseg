# fafbseg (development version)

# fafbseg 0.8.2

* Much faster `flywire_xyz2id()` supervoxel id mapping using spine service (#44)
* Add new `flywire_leaves()` function to find supervoxels in a neuron (#43)
* Fixing typos and adding doi for TEASAR (#41 by @mmc46)
* Fixing 2 things in overlay mesh+skeleton example 
(#40 by @mmc46)
* Fix flywire_xyz2id eg in doc 'The natverse and flywire meshes' returns an error (#39)
* Output of dr_fafbseg() points to wrong token helper function (#38 by @mmc46)

You can see the [full list of closed issues](https://github.com/natverse/fafbseg/issues?q=closed%3A2020-10-25..2020-11-12+)
on GitHub.

# fafbseg 0.8.1

* New function `skeletor()` to skeletonise meshes especially flywire neurons
  based on the Python package of the [same name](https://github.com/schlegelp/skeletor) (#35).
* Large speed up for `flywire_rootid()` (#37)
* Fix `flywire_xyz2id()` so that it can actually find supervoxels (as well as root ids) (#37)
* function `dr_fafbseg()` to give a status update on your installation (#36)
* `read_cloudvolume_meshes()` can now accept  flywire URLs (which will be expanded if necessary to define the segments to download)

You can see the [full list of closed issues](https://github.com/natverse/fafbseg/issues?q=closed%3A2020-09-13..2020-10-25+)
on GitHub.

# fafbseg 0.8.0

* Support for some basic flywire API calls #31, #34
* Better integrate flywire api features 
* Fix/flywire coordinate errors 
* Fix error in fafb2flywire during xform_brain #30, #32, #33
* Switch to transform-service API on spine 
* Add support for FANC3-FANC4 transforms #28 
* switch back to wire3d #27
* Add fafb2flywire i.e. the inverse of the original FlyWire->FAFB transformation #26
* Support for reading flywire meshes without cloudvolume #25
* Flywire API streamlining (@SridharJagannathan) #22
* Teach read_cloudvolume_meshes to work for flywire URLs #20
* `ngl_segments()` needs to be able to expand flywire URLs #16

You can see the [full list of closed issues](https://github.com/natverse/fafbseg/issues?q=closed%3A2020-07-28..2020-09-12+)
on GitHub.


# fafbseg 0.7.0

* basic support for reading meshes via CloudVolume e.g. to fetch neurons from
  flywire.ai. See `read_cloudvolume_meshes()` for details including the required
  Python setup.
* very basic support for skeletonising neurons meshed (e.g. flywire neurons)
  via `meshparty_skeletonize()`
* add `flywire2fafb()` to support for transforming neurons FlyWire->FAFB
  (and with some loss of accuracy in the reverse)  (#11)
* includes a fix for NaN values in return (#12,#13)
* much faster FlyWire->FAFB transformation, up to millions of points per minute
  (#24)
* moved repo to https://github.com/natverse/fafbseg (#19)

# fafbseg 0.6.5

* default chunksize for `brainmaps_xyz2id()` reduce to 200 to reflect API 
  changes.
* new functions `choose_segmentation()` and `with_segmentation()` to choose
  default auto-segmentation
* New Shiny app (see README and https://jefferislab.shinyapps.io/CATMAID-Neuroglancer-Converter/)
* give `brainmaps_fetch()` a generic cache option and the ability to clear the
  cache with `brainmaps_clear_cache()` (#8)
* give `brainmaps_fetch()` a retry option to help with sporadic timeouts (#9)
* give `catmaid2ngl()` a chunksize option that can be used to reduce timeout
  issues.
* simplify package `.onLoad` and retire fafbseg.divisor option 
  (now calculated automatically rather than being a user option)
* simplify internal `brainmaps_voxdims` function using cache mechanism

# fafbseg 0.6.4

* fix bug revealed by latest public version of zip package
* fix missing import of `dplyr::n()` in `find_topn()`

# fafbseg 0.6.3

* Give `find_topn()` and `read_top()` ability to return segments in 
  increasing (rather than decreasing) size order.

# fafbseg 0.6.2

* fix `ngl_encode_url()` when only one segment in scene (#5)
* fix handling or neuroglancer URLs with two segment sources (#4)

# fafbseg 0.6.1

* New `catmaid2ngl()` function and methods, a high level approach to converting
  URLs, neurons etc to representations based on Neuroglancer / brainmaps data.

# fafbseg 0.6.0

* default remote volume for `brainmaps_xyz()` has been updated to
  `brainmaps://772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2_flatreseg2_skeletons32`
* this volume is also used for `read.neurons.brainmaps()`

# fafbseg 0.5.5

* `brainmaps_skeleton()` and `read.neurons.brainmaps()` now use a brainmaps
  URI to specify the remote skeleton source.
* new package option `fafbseg.skeletonuri` to specify default remote skeleton 
  source
* give `brainmaps_xyz2id()` a `chunksize` argument to handle more query points 
  than the remote API will accept in a single call.

# fafbseg 0.5.4

* add `read.neuron.brainmaps()` and `read.neurons.brainmaps()` to read skeletons 
  into nat neuron objects over the web.
* this depends on the lower level `brainmaps_skeleton()` function.
* teach `find_merge_groups()` to return segment ids

# fafbseg 0.5.3

* `compare_ng_neuron()`: add pointsize and sample_dots args
* doc / example tweaks

# fafbseg 0.5.2

* Support for reading 3D meshes directly from brainmaps API via `read_brainmaps_meshes()`

# fafbseg 0.5.1

* export `brainmaps_xyz2id()` (#2)

# fafbseg 0.5.0

* Add `brainmaps_auth()`, `brainmaps_fetch()` to talk to brainmaps API
* Add `brainmaps_xyz2id()` to convert arbitrary XYZ locations to the corresponding
  segment id.
* Teach `find_merged_segments()` to return the merge groups

# fafbseg 0.4.1

* additional functions for reading/processing merge (agglomeration) information
  including `read_mergeinfo()`, `make_merge_graph()`, `merge_graph_components()`. These were
  used to generate the `fafbsegdata` package and could be useful in their own 
  right.

# fafbseg 0.4.0

* Add `find_merged_segments()` to find all the raw segments that are candidate 
  merges from the agglomeration runs. This functionality depends on the new 
  fafbsegdata package, which should be installed automatically as a suggested
  dependency.
* Switch default zip file divisor to 1E6 (and actually check the contents of a 
  zip file to accommodate a new default in Peter Li's 2018-10-02 skeleton 
  release (`fafb14_v00c_split3xfill2x_skeleton32nm512_nnconn75_thresh1000_sparse250`)

# fafbseg 0.3.5

* fix bug in `read_segments()` `datafrac` argument logic - was only reading largest fragment
* `datafrac` only applies to files > `minfilesize` 
* add internal `skelsforsegments()` function with progress and use it in `read_segments2()`
* Turn on `zip_list` memoisation again but with a 5 min cache timeout - can speed
  up read_segments2 considerably when there are multiple segments inside the 
  same zip file.

# fafbseg 0.3.4

* turn off zip_list memoisation to save memory (#1)

# fafbseg 0.3.3

* teach `read_segments2()` to read only the top n percent of skeletons by file size
* additional option to read coordinates only
* fix handling of zip files with >65535 files (see https://github.com/r-lib/zip/issues/11) 
  by making use of new ziplist64 package when available

# fafbseg 0.3.2

* export `ngl_decode_scene()` function to parse neuroglancer URLs or JSON scene
  specifications (in turn allowing these to be opened in CATMAID)
* fail better when skeleton zip files are missing
* fix bug in reading multiple bit64 ids

# fafbseg 0.3.1

* export `open_fafb_ngl()`
* teach `open_fafb_ngl` to produce coordinate string to paste into Neuroglancer
* add `ngl_segments()` to extract segment ids from diverse scene specifications such
  as URLs or JSON fragments copied from Neuroglancer web page

# fafbseg 0.3.0

* `read_segments2()` (in memory zip extraction) recommended
* add `find_topn()` to find biggest segments in zip files
* teach `read_topn()` to cope with multiple zip files
* fix sample URL to point to correct segmentation (`fafb_v14_16nm_v00c_split3xfill2`)
  that is compatible with the skeletons we have received.

# fafbseg 0.2.0

* add support for reading neurons from zipped skeleton files
* support for mapping ids to files and vice versa
* simplify setup for neuroglancer URLs
* improved docs

# fafbseg 0.1.0

* first version
