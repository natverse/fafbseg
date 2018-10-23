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
  used to generate the fafbsegdata package and could be useful in their own 
  right.

# fafbseg 0.4.0

* Add `find_merged_segments()` to find all the raw segments that are candidate 
  merges from the agglomeration runs. This functionality depends on the new 
  fafbsegdata package, which should be installed automatically as a suggested
  dependency.
* Switch default zip file divisor to 1E6 (and actually check the contents of a 
  zip file to accommodate a new default in Peter Li's 2018-10-02 skeleton 
  release (fafb14_v00c_split3xfill2x_skeleton32nm512_nnconn75_thresh1000_sparse250)

# fafbseg 0.3.5

* fix bug in `read_segments()` `datafrac` argument logic - was only reading largest fragment
* datafrac only applies to files > minfilesize 
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
* fix sample URL to point to correct segmentation (fafb_v14_16nm_v00c_split3xfill2)
  that is compatible with the skeletons we have received.

# fafbseg 0.2.0

* add support for reading neurons from zipped skeleton files
* support for mapping ids to files and vice versa
* simplify setup for neuroglancer URLs
* improved docs

# fafbseg 0.1.0

* first version
