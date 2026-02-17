# Changelog

## fafbseg 0.15.5

- [`cam_meta()`](https://natverse.org/fafbseg/reference/cam_meta.md) now
  uses
  [`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md)
  for much faster repeated queries, with pass-through control of cache
  strategy via `...`
- [`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md)
  gains a `limit` parameter
- [`flytable_alltables()`](https://natverse.org/fafbseg/reference/flytable_login.md)
  now uses disk caching with smart invalidation

## fafbseg 0.15.4

- new
  [`flytable_cached_table()`](https://natverse.org/fafbseg/reference/flytable_cached_table.md)
  for disk-cached table access with delta sync by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/226>
- new [`cam_meta()`](https://natverse.org/fafbseg/reference/cam_meta.md)
  function to give more generic access to Cambridge seatable by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/224>
- [`flytable_list_selected()`](https://natverse.org/fafbseg/reference/flytable_list_selected.md)
  now protects field names in queries
- [`cam_meta()`](https://natverse.org/fafbseg/reference/cam_meta.md)
  supports “/DNa02” type queries

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.15.3>…v0.15.4

## fafbseg 0.15.3

- only use spine for fafb datasets by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/216>
- Minor addition and fixes to code related to transmitters,
  skeletonisation and CAVE tokens by
  [@alexanderbates](https://github.com/alexanderbates) in
  <https://github.com/natverse/fafbseg/pull/213>
- teach xform.ngscene to support multisource layers by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/219>
- don’t mangle 64 bit ids in ngl annotations by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/217>
- flywire_xyz2id: use flywire_voxdims() for default by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/220>
- Feature/neuroglancer 2025 scenes by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/222>
- Feature/princeton synapses by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/fafbseg/pull/223>

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.15.2>…v0.15.3

## fafbseg 0.15.2

- Work around issue in curl 8.7.1 by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/210>
- Small speed up in writing to
  [`flywire_leaves()`](https://natverse.org/fafbseg/reference/flywire_leaves.md)
  cache by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/211>
- add cell_sub_class to metadata columns by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/214>
- support for support for real github urls (inc branches) (9d7a6c4)
- [`flywire_cave_query()`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)
  set `live=TRUE` if we provided a timestamp (e65e966)

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.15.1>…v0.15.2

## fafbseg 0.15.1

- Support for generic si git repositories by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/209> This will be helpful
  for distributing annotations for other papers
- add flywire_user_info() by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/208>

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.15.0>…v0.15.1

## fafbseg 0.15.0

This contains substantial under the hood changes in
[`flywire_cave_query()`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)
which may be breaking in some instances, based on fixes of my
understanding of CAVE live queries.

Changes in
[`flywire_partner_summary()`](https://natverse.org/fafbseg/reference/flywire_partners.md)
are also significant in allowing considerably faster queries for
multiple input neurons with `cave` now the default query method.

### What’s Changed

- [`flywire_cave_query()`](https://natverse.org/fafbseg/reference/flywire_cave_query.md):
  add livelive, fix live query by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/206>
- Faster
  [`flywire_partner_summary()`](https://natverse.org/fafbseg/reference/flywire_partners.md)
  queries by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/205>
- Fix/actions aug 2024 by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/207>

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.14.2>…v0.15.0

## fafbseg 0.14.2

This is a pre-release in preparation for a major (0.15.0) release and
does feature some API changes in
[`flywire_partner_summary()`](https://natverse.org/fafbseg/reference/flywire_partners.md)
(motivated by fanc/banc support in the coconatfly package).

### What’s Changed

- Streamline
  [`flywire_partner_summary()`](https://natverse.org/fafbseg/reference/flywire_partners.md)
  by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/203>
- [`flywire_cave_query()`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)
  query enhancements inc `select_columns` to speed up partner queries by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/202>
- fix bad gateway error for `nucleus_table_info()` and friends by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/204>
- Direct support for flyem shorturls including via tinyurl by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/201>

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.14.1>…v0.14.2

## fafbseg 0.14.1

- teach
  [`flywire_ids()`](https://natverse.org/fafbseg/reference/flywire_ids.md)
  to accept any whitespace by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/fafbseg/pull/198>
- Teach
  [`flywire_ids()`](https://natverse.org/fafbseg/reference/flywire_ids.md)
  to accept file argument by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/199>
- fix downloading v630 annotations by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/200>
- dr_fafbseg - summarise options in
  [`dr_fafbseg()`](https://natverse.org/fafbseg/reference/dr_fafbseg.md)
  (1eabc55b)

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.14.0>…v0.14.1

## fafbseg 0.14.0

This major release provides support for the v783 materialisation that we
expect to accompany the public release of the FlyWire dataset. It is
also features numerous bug fixes/usability improvements.

- flywire_partner_summary: support versions/timestamps by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/178>
- Fix rawcoords in flywire-nuclei by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/179>
- Fix: fetch static metadata when no cell type by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/180>
- simplify install on github by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/fafbseg/pull/181>
- fix stale flytable base cache by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/175>
- Feature/GitHub pkgdown by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/182>
- Fix/standardise userdir by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/183>
- Respect user selected data version by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/184>
- teach flywire_timestamp to accept ‘now’ by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/185>
- full support for arbitrary datastacks for read_l2skel by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/186>
- give simple_python extra option for meshparty et al by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/188>
- Feature cave views by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/190>
- Fix/flywire latest badids by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/fafbseg/pull/191>
- support for materialisation 783 artefacts by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/192>
- changes for fafbseg-py v3 by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/fafbseg/pull/194>
- Fix/flytable shared tables by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/fafbseg/pull/195>
- flytable_base4table too conservative listing tables by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/197>
- Fix/ngl annotation cols by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/196>

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.13.0>…v0.14.0

## fafbseg 0.13.0

- Feature optic info table by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/fafbseg/pull/174>
- Fix fafbseg inner_join / tests by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/176>
- Support for released cell type / connectivity data by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/177>
- Add support for ito_lee_hemilineage in searches
  (d17a6edfcef0aa2b709797d33117b21c4c59b203)
- Give add_celltype_info a suffix argument
  (136ecab3ecc149f683a3f81cf1c6cc02ce4c99c8)
- updates tests for FAFB LR swap
  (afe883e86a8f958437c178a38d0878d47cc7298d)
- Fix flywire_latestids for 64 bit root ids
  (aa8e560216af0117aed55fb49b222deccd385ccc)

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.12.0>…v0.13.0

## fafbseg 0.12.0

### What’s Changed

- Add support for cached flywire connectome dumps (see
  [`flywire_connectome_data()`](https://natverse.org/fafbseg/reference/flywire_connectome_data.md)
  and
  [`flywire_partner_summary2()`](https://natverse.org/fafbseg/reference/flywire_partner_summary2.md)
  and
  [`flywire_adjacency_matrix2()`](https://natverse.org/fafbseg/reference/flywire_adjacency_matrix2.md))
  by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/170>
- [`flywire_ids()`](https://natverse.org/fafbseg/reference/flywire_ids.md)
  (and most fafbseg functions that take ids as inputs) now supports full
  regular expressions to define id queries. For example
  `/type:MBON0[24]` (fc9df01b69eb5c540bc0bb1ebec89aad4621c695).
- [`flywire_islatest()`](https://natverse.org/fafbseg/reference/flywire_islatest.md)
  has a cache option when using a specific version/timestamp
  (4b284d7d86b7e1a6da980c521a282fa693f71456). This means that
  [`flywire_updateids()`](https://natverse.org/fafbseg/reference/flywire_updateids.md)
  and
  [`flywire_partner_summary2()`](https://natverse.org/fafbseg/reference/flywire_partner_summary2.md)
  can be very fast when using a fixed version.
- export
  [`read_l2dp()`](https://natverse.org/fafbseg/reference/read_l2skel.md)
  and
  [`read_l2skel()`](https://natverse.org/fafbseg/reference/read_l2skel.md)
- Neuroglancer URL handling (remember baseurl, support tinyurl) by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/167>
- Make
  [`flywire_raw2nm()`](https://natverse.org/fafbseg/reference/flywire_voxdims.md)
  use vox dims if supplied by [@jefferis](https://github.com/jefferis)
  in <https://github.com/natverse/fafbseg/pull/169>
- Add
  [`xform.ngscene()`](https://natverse.org/fafbseg/reference/xform.ngscene.md)
  by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/168>
- Python improvements to standardise on R-specific miniconda by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/171>. This includes a new
  `fafbseg.condaenv` option if you want to specify a non-standard
  miniconda environment.
- Support flytable super_class by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/172>
- Export annotations to neuroglancer info files (see
  [`write_nginfo()`](https://natverse.org/fafbseg/reference/write_nginfo.md))
  by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/173>
- add `flytable_meta` as a simple interface to get flytable cell type
  information for a set of flywire_ids

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.11.1>…v0.12.0

## fafbseg 0.11.1

- Fix nasty bug in adjacency matrix when inputids!=outputids by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/165>. Also ~2x speedup for
  many use cases.

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.11.0>…v0.11.1

## fafbseg 0.11.0

- Support for flytable cell type queries and timestamped id updates by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/163>
- Fix flytable maximum cells error by adjusting chunksize by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/147>
- respect chunksize when provided in flytable_list_rows by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/148>
- Add support for L2 ids and speed up flywire_latestid by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/151>
- Better reporting on token locations by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/154>
- Feature/ng annotations by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/153>
- Fix doc typo by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/156>
- Fixes for flywire_cave_query timestamp handling by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/159>
- Feature navis+fafbseg-py for reading L2 skeletons by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/160>
- Teach flywire_cave_query about query filter dicts by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/162>

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.10.0>…v0.11.0

## fafbseg 0.10.0

The key change is to react to a breaking change in the URL to access
spine services.

- Feature flytable_append_rows by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/132>
- Update Buhmann ref date by [@emilkind](https://github.com/emilkind) in
  <https://github.com/natverse/fafbseg/pull/138>
- Feature/misc flywire updates by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/139>
- Add flywire_nuclei to pkgdown by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/140>
- More flytable enhancements / fixes by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/141>
- Better handling of NAs in when handling body ids by
  [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/142>
- Fix/spine rename by [@jefferis](https://github.com/jefferis) in
  <https://github.com/natverse/fafbseg/pull/146>
- spine.janelia.org -\> services.itanna.io by
  [@perlman](https://github.com/perlman) in
  <https://github.com/natverse/fafbseg/pull/144>
- changed top.nt and top.p to top_nt and top_p by
  [@alexanderbates](https://github.com/alexanderbates) in
  <https://github.com/natverse/fafbseg/pull/143>

### New Contributors

- @emilkind made their first contribution in
  <https://github.com/natverse/fafbseg/pull/138>
- @perlman made their first contribution in
  <https://github.com/natverse/fafbseg/pull/144>

**Full Changelog**:
<https://github.com/natverse/fafbseg/compare/v0.9.4>…v0.10.0

## fafbseg 0.9.4

- add support for the Seung/Allen python CAVE including
  [`flywire_cave_client()`](https://natverse.org/fafbseg/reference/flywire_cave_client.md)
  [`flywire_cave_query()`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)
  ([\#122](https://github.com/natverse/fafbseg/issues/122))
- add
  [`flywire_updateids()`](https://natverse.org/fafbseg/reference/flywire_updateids.md)
  ([\#127](https://github.com/natverse/fafbseg/issues/127))
- also more robustness to handling of NAs in
  [`flywire_rootid()`](https://natverse.org/fafbseg/reference/flywire_rootid.md)
- first implementation of `flytable_*` functions including
  [`flytable_query()`](https://natverse.org/fafbseg/reference/flytable-queries.md)
  [`flytable_list_rows()`](https://natverse.org/fafbseg/reference/flytable-queries.md)
  [`flytable_login()`](https://natverse.org/fafbseg/reference/flytable_login.md)
  ([\#126](https://github.com/natverse/fafbseg/issues/126))
- note about SSL certificate problem with some flywire/fanc queries bug
  ([\#125](https://github.com/natverse/fafbseg/issues/125))
- Add ids type info to
  [`flywire_partners()`](https://natverse.org/fafbseg/reference/flywire_partners.md)

<https://github.com/natverse/fafbseg/issues?q=closed%3A2021-09-20>..2021-10-29+

## fafbseg 0.9.3 (pre-release version)

This is a pre-release without full release notes, principally to support
a new dependant package, fancr.

- new
  [`flywire_scene()`](https://natverse.org/fafbseg/reference/flywire_scene.md)
  function for simple specification of flywire neuroglancer scenes.
- add a number of features to allow programmatic manipulation of
  neuroglancer scenes. This includes `ngl_segments<-()` to replace the
  segments in a scene `+.ngscene()` and `-.ngscene()` to add or remove
  segments from a scene and
  [`as.character.ngscene()`](https://natverse.org/fafbseg/reference/ngl_encode_url.md)
  to convert a scene object to a URL. See examples in
  [`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  and
  [`ngl_decode_scene()`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md)
  for further details
  ([\#55](https://github.com/natverse/fafbseg/issues/55),
  [\#58](https://github.com/natverse/fafbseg/issues/58)).
- update
  [`flywire_set_token()`](https://natverse.org/fafbseg/reference/flywire_set_token.md)
  to support new cave and zetta tokens (for FANC dataset)
  ([\#119](https://github.com/natverse/fafbseg/issues/119))
- expose
  [`chunkedgraph_token()`](https://natverse.org/fafbseg/reference/flywire_set_token.md)
  function ([\#119](https://github.com/natverse/fafbseg/issues/119))
- Further updates to token handling to handle FANC/FlyWire
  ([\#121](https://github.com/natverse/fafbseg/issues/121))
- Private function `check_cloudvolume_reticulate` now supports a minimum
  version
- Handle 1 node neurons
  ([\#120](https://github.com/natverse/fafbseg/issues/120))
- Basic support for the Python
  [caveclient](https://caveclient.readthedocs.io/) package. This enables
  fetching a range of annotation data from the CAVE (Connectome
  Annotation Versioning Engine) API including position of nuclei or user
  annotations. One notable improvement is more rapid fetching of
  synaptic partner information. This is implemented by the new
  `method="cave"` option for
  [`flywire_partner_summary()`](https://natverse.org/fafbseg/reference/flywire_partners.md)
  ([\#122](https://github.com/natverse/fafbseg/issues/122)).

<https://github.com/natverse/fafbseg/issues?q=closed%3A2020-12-27>..2021-09-20+

## fafbseg 0.9

The version change to 0.9 reflects some small but potentially breaking
changes in user visible behaviour. In particular
[`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md)
now returns as character by default (to protect FlyWire ids, which
cannot be expressed as 8 byte floating point values).

- Add
  [`flywire_latestid()`](https://natverse.org/fafbseg/reference/flywire_latestid.md)
  function to map any (past) root id to the current root id.
  Specifically it it identifies the neuron inheriting the largest number
  of super voxels from the object specified by the input root id.
- Support for Google ffn1 20200412 segmentation
  ([\#14](https://github.com/natverse/fafbseg/issues/14),
  [\#46](https://github.com/natverse/fafbseg/issues/46))
- Make
  [`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  return ids as character vector, rather than numeric, by default. Also
  exclude hidden ids by default
  ([\#50](https://github.com/natverse/fafbseg/issues/50),
  [\#51](https://github.com/natverse/fafbseg/issues/51)).
- Allow
  [`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  (and flywire\_\*) fns to accept URLs with surface meshes. Previously
  these would cause an error
  ([\#52](https://github.com/natverse/fafbseg/issues/52))
- Fix
  [`ngl_encode_url()`](https://natverse.org/fafbseg/reference/ngl_encode_url.md)
  so that all tested flywire scenes can be parsed in R and then
  converted back to valid URLs that can be opened in flywire.ai
  ([\#53](https://github.com/natverse/fafbseg/issues/53))
- Add print.ngscene method to summarise neuroglancer scenes in the
  console ([\#54](https://github.com/natverse/fafbseg/issues/54))
- [`flywire_cloudvolume()`](https://natverse.org/fafbseg/reference/flywire_cloudvolume.md)
  enables improved low-level support for flywire+cloudvolume (by
  exposing a reusable cloudvolume object)
  ([\#48](https://github.com/natverse/fafbseg/issues/48))

[full list of closed
issues](https://github.com/natverse/fafbseg/issues?q=closed%3A2020-11-13..2020-12-27+)
on GitHub. \# fafbseg 0.8.2

- Much faster
  [`flywire_xyz2id()`](https://natverse.org/fafbseg/reference/flywire_xyz2id.md)
  supervoxel id mapping using spine service
  ([\#44](https://github.com/natverse/fafbseg/issues/44))
- Add new
  [`flywire_leaves()`](https://natverse.org/fafbseg/reference/flywire_leaves.md)
  function to find supervoxels in a neuron
  ([\#43](https://github.com/natverse/fafbseg/issues/43))
- Fixing typos and adding doi for TEASAR
  ([\#41](https://github.com/natverse/fafbseg/issues/41) by
  [@mmc46](https://github.com/mmc46))
- Fixing 2 things in overlay mesh+skeleton example
  ([\#40](https://github.com/natverse/fafbseg/issues/40) by
  [@mmc46](https://github.com/mmc46))
- Fix flywire_xyz2id eg in doc ‘The natverse and flywire meshes’ returns
  an error ([\#39](https://github.com/natverse/fafbseg/issues/39))
- Output of dr_fafbseg() points to wrong token helper function
  ([\#38](https://github.com/natverse/fafbseg/issues/38) by
  [@mmc46](https://github.com/mmc46))

You can see the [full list of closed
issues](https://github.com/natverse/fafbseg/issues?q=closed%3A2020-10-25..2020-11-12+)
on GitHub.

## fafbseg 0.8.1

- New function
  [`skeletor()`](https://natverse.org/fafbseg/reference/skeletor.md) to
  skeletonise meshes especially flywire neurons based on the Python
  package of the [same name](https://github.com/schlegelp/skeletor)
  ([\#35](https://github.com/natverse/fafbseg/issues/35)).
- Large speed up for
  [`flywire_rootid()`](https://natverse.org/fafbseg/reference/flywire_rootid.md)
  ([\#37](https://github.com/natverse/fafbseg/issues/37))
- Fix
  [`flywire_xyz2id()`](https://natverse.org/fafbseg/reference/flywire_xyz2id.md)
  so that it can actually find supervoxels (as well as root ids)
  ([\#37](https://github.com/natverse/fafbseg/issues/37))
- function
  [`dr_fafbseg()`](https://natverse.org/fafbseg/reference/dr_fafbseg.md)
  to give a status update on your installation
  ([\#36](https://github.com/natverse/fafbseg/issues/36))
- [`read_cloudvolume_meshes()`](https://natverse.org/fafbseg/reference/read_cloudvolume_meshes.md)
  can now accept flywire URLs (which will be expanded if necessary to
  define the segments to download)

You can see the [full list of closed
issues](https://github.com/natverse/fafbseg/issues?q=closed%3A2020-09-13..2020-10-25+)
on GitHub.

## fafbseg 0.8.0

- Support for some basic flywire API calls
  [\#31](https://github.com/natverse/fafbseg/issues/31),
  [\#34](https://github.com/natverse/fafbseg/issues/34)
- Better integrate flywire api features
- Fix/flywire coordinate errors
- Fix error in
  [`fafb2flywire()`](https://natverse.org/fafbseg/reference/flywire2fafb.md)
  during `xform_brain`
  [\#30](https://github.com/natverse/fafbseg/issues/30),
  [\#32](https://github.com/natverse/fafbseg/issues/32),
  [\#33](https://github.com/natverse/fafbseg/issues/33)
- Switch to transform-service API on spine
- Add support for FANC3-FANC4 transforms
  [\#28](https://github.com/natverse/fafbseg/issues/28)
- switch back to `wire3d`
  [\#27](https://github.com/natverse/fafbseg/issues/27)
- Add
  [`fafb2flywire()`](https://natverse.org/fafbseg/reference/flywire2fafb.md)
  i.e. the inverse of the original FlyWire-\>FAFB transformation
  [\#26](https://github.com/natverse/fafbseg/issues/26)
- Support for reading flywire meshes without cloudvolume
  [\#25](https://github.com/natverse/fafbseg/issues/25)
- Flywire API streamlining
  ([@SridharJagannathan](https://github.com/SridharJagannathan))
  [\#22](https://github.com/natverse/fafbseg/issues/22)
- Teach
  [`read_cloudvolume_meshes()`](https://natverse.org/fafbseg/reference/read_cloudvolume_meshes.md)
  to work for flywire URLs
  [\#20](https://github.com/natverse/fafbseg/issues/20)
- [`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  needs to be able to expand flywire URLs
  [\#16](https://github.com/natverse/fafbseg/issues/16)

You can see the [full list of closed
issues](https://github.com/natverse/fafbseg/issues?q=closed%3A2020-07-28..2020-09-12+)
on GitHub.

## fafbseg 0.7.0

- basic support for reading meshes via CloudVolume e.g. to fetch neurons
  from flywire.ai. See
  [`read_cloudvolume_meshes()`](https://natverse.org/fafbseg/reference/read_cloudvolume_meshes.md)
  for details including the required Python setup.
- very basic support for skeletonising neurons meshed (e.g. flywire
  neurons) via
  [`meshparty_skeletonize()`](https://natverse.org/fafbseg/reference/meshparty_skeletonize.md)
- add
  [`flywire2fafb()`](https://natverse.org/fafbseg/reference/flywire2fafb.md)
  to support for transforming neurons FlyWire-\>FAFB (and with some loss
  of accuracy in the reverse)
  ([\#11](https://github.com/natverse/fafbseg/issues/11))
- includes a fix for NaN values in return
  ([\#12](https://github.com/natverse/fafbseg/issues/12),#13)
- much faster FlyWire-\>FAFB transformation, up to millions of points
  per minute ([\#24](https://github.com/natverse/fafbseg/issues/24))
- moved repo to <https://github.com/natverse/fafbseg>
  ([\#19](https://github.com/natverse/fafbseg/issues/19))

## fafbseg 0.6.5

- default chunksize for
  [`brainmaps_xyz2id()`](https://natverse.org/fafbseg/reference/brainmaps_xyz2id.md)
  reduce to 200 to reflect API changes.
- new functions
  [`choose_segmentation()`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  and
  [`with_segmentation()`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  to choose default auto-segmentation
- New Shiny app (see README and
  <https://jefferislab.shinyapps.io/CATMAID-Neuroglancer-Converter/>)
- give
  [`brainmaps_fetch()`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md)
  a generic cache option and the ability to clear the cache with
  [`brainmaps_clear_cache()`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md)
  ([\#8](https://github.com/natverse/fafbseg/issues/8))
- give
  [`brainmaps_fetch()`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md)
  a retry option to help with sporadic timeouts
  ([\#9](https://github.com/natverse/fafbseg/issues/9))
- give
  [`catmaid2ngl()`](https://natverse.org/fafbseg/reference/catmaid2ngl.md)
  a chunksize option that can be used to reduce timeout issues.
- simplify package `.onLoad` and retire fafbseg.divisor option (now
  calculated automatically rather than being a user option)
- simplify internal `brainmaps_voxdims` function using cache mechanism

## fafbseg 0.6.4

- fix bug revealed by latest public version of zip package
- fix missing import of
  [`dplyr::n()`](https://dplyr.tidyverse.org/reference/context.html) in
  [`find_topn()`](https://natverse.org/fafbseg/reference/read_topn.md)

## fafbseg 0.6.3

- Give
  [`find_topn()`](https://natverse.org/fafbseg/reference/read_topn.md)
  and `read_top()` ability to return segments in increasing (rather than
  decreasing) size order.

## fafbseg 0.6.2

- fix
  [`ngl_encode_url()`](https://natverse.org/fafbseg/reference/ngl_encode_url.md)
  when only one segment in scene
  ([\#5](https://github.com/natverse/fafbseg/issues/5))
- fix handling or neuroglancer URLs with two segment sources
  ([\#4](https://github.com/natverse/fafbseg/issues/4))

## fafbseg 0.6.1

- New
  [`catmaid2ngl()`](https://natverse.org/fafbseg/reference/catmaid2ngl.md)
  function and methods, a high level approach to converting URLs,
  neurons etc to representations based on Neuroglancer / brainmaps data.

## fafbseg 0.6.0

- default remote volume for `brainmaps_xyz()` has been updated to
  `brainmaps://772153499790:fafb_v14:fafb_v14_16nm_v00c_split3xfill2_flatreseg2_skeletons32`
- this volume is also used for
  [`read.neurons.brainmaps()`](https://natverse.org/fafbseg/reference/read.neuron.brainmaps.md)

## fafbseg 0.5.5

- [`brainmaps_skeleton()`](https://natverse.org/fafbseg/reference/brainmaps_skeleton.md)
  and
  [`read.neurons.brainmaps()`](https://natverse.org/fafbseg/reference/read.neuron.brainmaps.md)
  now use a brainmaps URI to specify the remote skeleton source.
- new package option `fafbseg.skeletonuri` to specify default remote
  skeleton source
- give
  [`brainmaps_xyz2id()`](https://natverse.org/fafbseg/reference/brainmaps_xyz2id.md)
  a `chunksize` argument to handle more query points than the remote API
  will accept in a single call.

## fafbseg 0.5.4

- add
  [`read.neuron.brainmaps()`](https://natverse.org/fafbseg/reference/read.neuron.brainmaps.md)
  and
  [`read.neurons.brainmaps()`](https://natverse.org/fafbseg/reference/read.neuron.brainmaps.md)
  to read skeletons into nat neuron objects over the web.
- this depends on the lower level
  [`brainmaps_skeleton()`](https://natverse.org/fafbseg/reference/brainmaps_skeleton.md)
  function.
- teach `find_merge_groups()` to return segment ids

## fafbseg 0.5.3

- [`compare_ng_neuron()`](https://natverse.org/fafbseg/reference/compare_ng_neuron.md):
  add pointsize and sample_dots args
- doc / example tweaks

## fafbseg 0.5.2

- Support for reading 3D meshes directly from brainmaps API via
  [`read_brainmaps_meshes()`](https://natverse.org/fafbseg/reference/read_brainmaps_meshes.md)

## fafbseg 0.5.1

- export
  [`brainmaps_xyz2id()`](https://natverse.org/fafbseg/reference/brainmaps_xyz2id.md)
  ([\#2](https://github.com/natverse/fafbseg/issues/2))

## fafbseg 0.5.0

- Add
  [`brainmaps_auth()`](https://natverse.org/fafbseg/reference/brainmaps_auth.md),
  [`brainmaps_fetch()`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md)
  to talk to brainmaps API
- Add
  [`brainmaps_xyz2id()`](https://natverse.org/fafbseg/reference/brainmaps_xyz2id.md)
  to convert arbitrary XYZ locations to the corresponding segment id.
- Teach
  [`find_merged_segments()`](https://natverse.org/fafbseg/reference/find_merged_segments.md)
  to return the merge groups

## fafbseg 0.4.1

- additional functions for reading/processing merge (agglomeration)
  information including
  [`read_mergeinfo()`](https://natverse.org/fafbseg/reference/read_mergeinfo.md),
  [`make_merge_graph()`](https://natverse.org/fafbseg/reference/make_merge_graph.md),
  [`merge_graph_components()`](https://natverse.org/fafbseg/reference/make_merge_graph.md).
  These were used to generate the `fafbsegdata` package and could be
  useful in their own right.

## fafbseg 0.4.0

- Add
  [`find_merged_segments()`](https://natverse.org/fafbseg/reference/find_merged_segments.md)
  to find all the raw segments that are candidate merges from the
  agglomeration runs. This functionality depends on the new fafbsegdata
  package, which should be installed automatically as a suggested
  dependency.
- Switch default zip file divisor to 1E6 (and actually check the
  contents of a zip file to accommodate a new default in Peter Li’s
  2018-10-02 skeleton release
  (`fafb14_v00c_split3xfill2x_skeleton32nm512_nnconn75_thresh1000_sparse250`)

## fafbseg 0.3.5

- fix bug in
  [`read_segments()`](https://natverse.org/fafbseg/reference/read_segments.md)
  `datafrac` argument logic - was only reading largest fragment
- `datafrac` only applies to files \> `minfilesize`
- add internal `skelsforsegments()` function with progress and use it in
  [`read_segments2()`](https://natverse.org/fafbseg/reference/read_segments.md)
- Turn on `zip_list` memoisation again but with a 5 min cache timeout -
  can speed up read_segments2 considerably when there are multiple
  segments inside the same zip file.

## fafbseg 0.3.4

- turn off zip_list memoisation to save memory
  ([\#1](https://github.com/natverse/fafbseg/issues/1))

## fafbseg 0.3.3

- teach
  [`read_segments2()`](https://natverse.org/fafbseg/reference/read_segments.md)
  to read only the top n percent of skeletons by file size
- additional option to read coordinates only
- fix handling of zip files with \>65535 files (see
  <https://github.com/r-lib/zip/issues/11>) by making use of new
  ziplist64 package when available

## fafbseg 0.3.2

- export
  [`ngl_decode_scene()`](https://natverse.org/fafbseg/reference/ngl_decode_scene.md)
  function to parse neuroglancer URLs or JSON scene specifications (in
  turn allowing these to be opened in CATMAID)
- fail better when skeleton zip files are missing
- fix bug in reading multiple bit64 ids

## fafbseg 0.3.1

- export
  [`open_fafb_ngl()`](https://natverse.org/fafbseg/reference/open_fafb_ngl.md)
- teach `open_fafb_ngl` to produce coordinate string to paste into
  Neuroglancer
- add
  [`ngl_segments()`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  to extract segment ids from diverse scene specifications such as URLs
  or JSON fragments copied from Neuroglancer web page

## fafbseg 0.3.0

- [`read_segments2()`](https://natverse.org/fafbseg/reference/read_segments.md)
  (in memory zip extraction) recommended
- add
  [`find_topn()`](https://natverse.org/fafbseg/reference/read_topn.md)
  to find biggest segments in zip files
- teach
  [`read_topn()`](https://natverse.org/fafbseg/reference/read_topn.md)
  to cope with multiple zip files
- fix sample URL to point to correct segmentation
  (`fafb_v14_16nm_v00c_split3xfill2`) that is compatible with the
  skeletons we have received.

## fafbseg 0.2.0

- add support for reading neurons from zipped skeleton files
- support for mapping ids to files and vice versa
- simplify setup for neuroglancer URLs
- improved docs

## fafbseg 0.1.0

- first version
