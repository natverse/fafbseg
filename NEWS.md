# fafbseg 0.11.1

* Fix nasty bug in adjacency matrix when inputids!=outputids by @jefferis in https://github.com/natverse/fafbseg/pull/165. Also ~2x speedup for many use cases.

**Full Changelog**: https://github.com/natverse/fafbseg/compare/v0.11.0...v0.11.1

# fafbseg 0.11.0

* Support for flytable cell type queries and timestamped id updates by @jefferis in https://github.com/natverse/fafbseg/pull/163
* Fix flytable maximum cells error by adjusting chunksize by @jefferis in https://github.com/natverse/fafbseg/pull/147
* respect chunksize when provided in flytable_list_rows by @jefferis in https://github.com/natverse/fafbseg/pull/148
* Add support for L2 ids and speed up flywire_latestid by @jefferis in https://github.com/natverse/fafbseg/pull/151
* Better reporting on token locations by @jefferis in https://github.com/natverse/fafbseg/pull/154
* Feature/ng annotations by @jefferis in https://github.com/natverse/fafbseg/pull/153
* Fix doc typo by @jefferis in https://github.com/natverse/fafbseg/pull/156
* Fixes for flywire_cave_query timestamp handling by @jefferis in https://github.com/natverse/fafbseg/pull/159
* Feature navis+fafbseg-py for reading L2 skeletons by @jefferis in https://github.com/natverse/fafbseg/pull/160
* Teach flywire_cave_query about query filter dicts by @jefferis in https://github.com/natverse/fafbseg/pull/162

**Full Changelog**: https://github.com/natverse/fafbseg/compare/v0.10.0...v0.11.0

# fafbseg 0.10.0

The key change is to react to a breaking change in the URL to access 
spine services. 

* Feature flytable_append_rows by @jefferis in https://github.com/natverse/fafbseg/pull/132
* Update Buhmann ref date by @emilkind in https://github.com/natverse/fafbseg/pull/138
* Feature/misc flywire updates by @jefferis in https://github.com/natverse/fafbseg/pull/139
* Add flywire_nuclei to pkgdown by @jefferis in https://github.com/natverse/fafbseg/pull/140
* More flytable enhancements / fixes by @jefferis in https://github.com/natverse/fafbseg/pull/141
* Better handling of NAs in when handling body ids by @jefferis in https://github.com/natverse/fafbseg/pull/142
* Fix/spine rename by @jefferis in https://github.com/natverse/fafbseg/pull/146
* spine.janelia.org -> services.itanna.io by @perlman in https://github.com/natverse/fafbseg/pull/144
* changed top.nt and top.p to top_nt and top_p by @alexanderbates in https://github.com/natverse/fafbseg/pull/143

## New Contributors
* @emilkind made their first contribution in https://github.com/natverse/fafbseg/pull/138
* @perlman made their first contribution in https://github.com/natverse/fafbseg/pull/144

**Full Changelog**: https://github.com/natverse/fafbseg/compare/v0.9.4...v0.10.0

# fafbseg 0.9.4

* add support for the Seung/Allen python CAVE including `flywire_cave_client()`
  `flywire_cave_query()` (#122)
* add `flywire_updateids()` (#127)
* also more robustness to handling of NAs in `flywire_rootid()`
* first implementation of `flytable_*` functions including `flytable_query()`
  `flytable_list_rows()` `flytable_login()` (#126)
* note about SSL certificate problem with some flywire/fanc queries bug (#125)
* Add ids type info to `flywire_partners()`

https://github.com/natverse/fafbseg/issues?q=closed%3A2021-09-20..2021-10-29+

# fafbseg 0.9.3 (pre-release version)

This is a pre-release without full release notes, principally to support a new dependant package, fancr.

* new `flywire_scene()` function for simple specification of flywire 
  neuroglancer scenes.
* add a number of features to allow programmatic manipulation of neuroglancer 
  scenes. This includes `ngl_segments<-()` to replace the segments in a scene
  `+.ngscene()` and `-.ngscene()` to add or remove segments from a scene and `as.character.ngscene()` to convert a scene object to a URL. See examples in `ngl_segments()` and `ngl_decode_scene()` for further details (#55, #58).
* update `flywire_set_token()` to support new cave and zetta tokens 
  (for FANC dataset) (#119)
* expose `chunkedgraph_token()` function (#119)
* Further updates to token handling to handle FANC/FlyWire (#121)
* Private function `check_cloudvolume_reticulate` now supports a minimum version
* Handle 1 node neurons (#120)
* Basic support for the Python [caveclient](https://caveclient.readthedocs.io/)
  package. 
  This enables fetching a range of annotation data from the CAVE 
  (Connectome Annotation Versioning Engine) API including position of nuclei or
  user annotations. 
  One notable improvement is more rapid fetching of synaptic partner information.
  This is implemented by the new `method="cave"` option for `flywire_partner_summary()` (#122).

https://github.com/natverse/fafbseg/issues?q=closed%3A2020-12-27..2021-09-20+

# fafbseg 0.9

The version change to 0.9 reflects some small but potentially breaking changes in user visible behaviour. 
In particular `ngl_segments()` now returns as character by default (to protect FlyWire ids, which cannot be expressed as 8 byte floating point values).

* Add `flywire_latestid()` function to map any (past) root id to the current root id. Specifically it it identifies the neuron inheriting the largest number of super voxels from the object specified by the input root id. 
* Support for Google ffn1 20200412 segmentation (#14, #46)
* Make `ngl_segments()` return ids as character vector, rather than numeric, by default. Also exclude hidden ids by default (#50, #51).
* Allow `ngl_segments()` (and flywire_*) fns to accept URLs with surface 
  meshes. Previously these would cause an error (#52)
* Fix `ngl_encode_url()` so that all tested flywire scenes can be parsed in R and then converted back to valid URLs that can be opened in flywire.ai (#53)
* Add print.ngscene method to summarise neuroglancer scenes in the console (#54) 
* `flywire_cloudvolume()` enables improved low-level support for flywire+cloudvolume (by exposing a reusable cloudvolume object) (#48)

[full list of closed issues](https://github.com/natverse/fafbseg/issues?q=closed%3A2020-11-13..2020-12-27+)
on GitHub.
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
* Fix error in `fafb2flywire()` during `xform_brain` #30, #32, #33
* Switch to transform-service API on spine 
* Add support for FANC3-FANC4 transforms #28 
* switch back to `wire3d` #27
* Add `fafb2flywire()` i.e. the inverse of the original FlyWire->FAFB transformation #26
* Support for reading flywire meshes without cloudvolume #25
* Flywire API streamlining (@SridharJagannathan) #22
* Teach `read_cloudvolume_meshes()` to work for flywire URLs #20
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
