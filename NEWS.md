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
