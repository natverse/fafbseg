# Query flywire/CAVE synapses within a bounding box or 3D surface

`flywire_synapse_query` fetches synapses from the CAVE materialisation
engine, optionally restricted to given pre-/postsynaptic partners and/or
to a spatial region defined by a bounding box or an arbitrary 3D
surface. It wraps the Python `fac$materialize$synapse_query` method.

## Usage

``` r
flywire_synapse_query(
  pre_ids = NULL,
  post_ids = NULL,
  bounding_box = NULL,
  bounding_box_column = c("post_pt_position", "pre_pt_position"),
  surf = NULL,
  invert_surf = FALSE,
  cleft.threshold = 0,
  remove_autapses = TRUE,
  synapse_table = NULL,
  version = NULL,
  timestamp = NULL,
  limit = NULL,
  fetch_all_rows = FALSE,
  slab_size = 500000L,
  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
  fafbseg_colnames = TRUE,
  progress = interactive(),
  ...
)
```

## Arguments

- pre_ids, post_ids:

  Optional root ids restricting the query to these presynaptic and/or
  postsynaptic partners (in any form acceptable to
  [`flywire_ids`](https://natverse.org/fafbseg/reference/flywire_ids.md)).

- bounding_box:

  A 2x3 matrix (rows min/max, columns x/y/z) or any object accepted by
  [`boundingbox`](https://rdrr.io/pkg/nat/man/boundingbox.html), in nm.
  Ignored when `surf` is supplied.

- bounding_box_column:

  Which synapse position column the bounding box/surface filter applies
  to (default `"post_pt_position"`).

- surf:

  A 3D region (mesh/surface/bounding box) inside which synapse positions
  must lie. Its bounding box restricts the server-side query and
  [`pointsinside`](https://rdrr.io/pkg/nat/man/pointsinside.html) does
  the exact filtering. See Details.

- invert_surf:

  When `TRUE` keep synapses *outside* `surf` rather than inside.

- cleft.threshold:

  Only keep synapses with `cleft_score` above this value (0-255, default
  0 i.e. no filtering).

- remove_autapses:

  Whether to drop synapses where pre and post root id are identical
  (default `TRUE`).

- synapse_table:

  Name of the synapse table. The default (`NULL`) resolves the
  datastack's synapse table automatically.

- version:

  An optional CAVE materialisation version number. See details and
  examples.

- timestamp:

  An optional timestamp as a string or POSIXct, interpreted as UTC when
  no timezone is specified.

- limit:

  Optional maximum number of rows to return.

- fetch_all_rows:

  Fetch all rows even when the server would otherwise truncate the
  result. Spatial queries are tiled into slabs; other queries are paged
  (see Details).

- slab_size:

  Target maximum number of rows per slab when tiling a large spatial
  `fetch_all_rows` query (default 5e5). See Details.

- datastack_name:

  defaults to the value selected by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  and to "flywire_fafb_production" when that is missing. See
  <https://global.daf-apis.com/info/> for other options.

- fafbseg_colnames:

  When `TRUE` (default) rename CAVE columns to fafbseg conventions (e.g.
  `pt_root_id` -\> `id`).

- progress:

  Whether to show a progress bar while fetching a large `fetch_all_rows`
  query (default
  [`interactive()`](https://rdrr.io/r/base/interactive.html)).

- ...:

  Additional arguments to the query method. See examples and details.

## Value

A `tibble` of synapses, or `NULL` when the server truncated an
over-large query (see `fetch_all_rows`).

## Details

Spatial arguments (`bounding_box`, `surf`) are supplied in
**nanometres**, the standard `nat` unit. The CAVE `synapse_query`
endpoint expects the bounding box in the synapse table's own voxel
resolution, so `flywire_synapse_query` looks that resolution up from the
table metadata (per datastack) and converts nm -\> voxels before the
call. Passing an nm bounding box straight to `synapse_query` would
silently inflate the region (e.g. ~4x in x/y and ~40x in z for FAFB) and
try to pull far more synapses than intended, which can crash the Python
session; converting first avoids that.

When `surf` is supplied its
[`boundingbox`](https://rdrr.io/pkg/nat/man/boundingbox.html) is used to
restrict the server-side query and the returned synapses are then
filtered with
[`pointsinside`](https://rdrr.io/pkg/nat/man/pointsinside.html) so that
only those whose `bounding_box_column` position actually lies inside the
surface are kept. `surf` may be any object `pointsinside` understands
([`hxsurf`](https://rdrr.io/pkg/nat/man/read.hxsurf.html),
[`mesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html),
[`boundingbox`](https://rdrr.io/pkg/nat/man/boundingbox.html)). Note
that `surf` must be in the same space as the segmentation (e.g. FlyWire
space, *not* FAFB14) since CAVE returns coordinates in that space.

The CAVE server truncates large results to a row limit and reports this
on the Python console rather than as an R warning. Set
`fetch_all_rows=TRUE` to retrieve everything. For a spatial query
(`bounding_box` or `surf`, with no `pre_ids`/`post_ids`) this is done by
*tiling*: a cheap server-side count picks a number of slabs so each
holds fewer than `slab_size` rows, the bounding box is split along its
longest axis, and each slab is fetched in a single un-paged request (a
slab that unexpectedly overflows is bisected and retried). This is much
faster than offset paging over one huge region, where the server
re-scans and discards the skipped rows on every page. Because
neighbouring slabs share an (inclusive) cut plane, the combined result
is de-duplicated by synapse id. Other `fetch_all_rows` queries (e.g.
restricted to `pre_ids`) fall back to offset paging with `limit` as the
page size (default 100000).

## See also

[`flywire_cave_query`](https://natverse.org/fafbseg/reference/flywire_cave_query.md)
for general CAVE table queries and
[`flywire_partners`](https://natverse.org/fafbseg/reference/flywire_partners.md)
/
[`flywire_partner_summary`](https://natverse.org/fafbseg/reference/flywire_partners.md)
for partner-centric synapse queries (which also support `surf`
filtering).

## Examples

``` r
if (FALSE) { # \dontrun{
# all synapses of a neuron within a neuropil surface (FlyWire space)
library(nat)
syn=flywire_synapse_query(pre_ids="720575940621039145",
  surf=subset(some.flywire.surf, "LH_R"))

# explicit bounding box in nm
bb=boundingbox(rbind(c(4e5,1.6e5,1e5), c(4.2e5,1.8e5,1.2e5)))
syn=flywire_synapse_query(bounding_box=bb)

# works with other datastacks e.g. from the aedes package
syn=flywire_synapse_query(pre_ids=id, surf=roi,
  datastack_name="my_datastack")
} # }
```
