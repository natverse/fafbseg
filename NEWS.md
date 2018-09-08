# fafbseg 0.3.3

* teach read_segments2 to read only the top n percent of skeletons by file size
* additional option to read coordinates only
* fix handling of zip files with >65535 files (see https://github.com/r-lib/zip/issues/11) 
  by making use of new ziplist64 package when available

# fafbseg 0.3.2

* export ngl_decode_scene function to parse neuroglancer URLs or JSON scene
  specifications (in turn allowing these to be opened in CATMAID)
* fail better when skeleton zip files are missing
* fix bug in reading multiple bit64 ids

# fafbseg 0.3.1

* export open_fafb_ngl
* teach open_fafb_ngl to produce coordinate string to paste into Neuroglancer
* add ngl_segments to extract segment ids from diverse scene specifications such
  as URLs or JSON fragments copied from Neuroglancer web page

# fafbseg 0.3.0

* read_segments2 (in memory zip extraction) recommended
* add find_topn to find biggest segments in zip files
* teach read_topn to cope with multiple zip files
* fix sample URL to point to correct segmentation (fafb_v14_16nm_v00c_split3xfill2)
  that is compatible with the skeletons we have received.

# fafbseg 0.2.0

* add support for reading neurons from zipped skeleton files
* support for mapping ids to files and vice versa
* simplify setup for neuroglancer URLs
* improved docs

# fafbseg 0.1.0

* first version
