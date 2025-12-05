# Find all the supervoxel (leaf) ids that are part of a FlyWire object

This workhorse function underlies the ability to define synaptic
connections and neurotransmitter predictions for flywire neurons.

## Usage

``` r
flywire_leaves(
  x,
  cloudvolume.url = NULL,
  integer64 = FALSE,
  mip = 0L,
  bbox = NULL,
  cache = TRUE,
  ...
)
```

## Arguments

- x:

  One or more FlyWire segment ids

- cloudvolume.url:

  URL for CloudVolume to fetch segmentation image data. The default
  value of NULL chooses the flywire production segmentation dataset.

- integer64:

  Whether to return ids as integer64 type (the default, more compact but
  a little fragile) rather than character (when `FALSE`).

- mip:

  The mip level for the segmentation (expert use only)

- bbox:

  The bounding box within which to find supervoxels (default = `NULL`
  for whole brain. Expert use only.)

- cache:

  Whether to cache the results of flywire_leaves calls. See details.

- ...:

  Additional arguments passed to `pbsapply` and eventually
  [`flywire_fetch`](https://natverse.org/fafbseg/reference/flywire_fetch.md)
  when `method="flywire"` OR to `cv$CloudVolume` when
  `method="cloudvolume"`

## Details

By default repeated calls to `flywire_leaves` are cached on disk. This
functionality is provided by the `cachem` package now used by the
`memoise` package. By default the cache will expand up to 1.5GB and then
start pruning on a least recently used basis (LRU). 1.5GB might store
10-20,000 results for `flywire_leaves` calls depending on the size of
the corresponding neurons.

Since each root id can map to hundreds of thousands of supervoxel ids,
there are space implications. In order to save space, the results are
stored as compressed 64 bit integers (which are ~30x smaller than
character vectors). The compression step does add an extra ~ 5 but is
100x + faster on a cache hit. The default compression is based on the
suggested brotli library if available, gzip otherwise.

There is functionality for a memory cache on top of the disk cache, but
this is not currently exposed as the disk read time appears small
compared with the time for uncompressing and other overheads.

The cache can be controlled by two package options:

- `fafbseg.cachedir` The location on disk. If not previously set, it is
  set to an appropriate user folder on package load using
  `rappdirs::`[`user_data_dir`](https://rappdirs.r-lib.org/reference/user_data_dir.html).
  Note that the cache for this function will be located inside a folder
  called `flywire_leaves`.

- `fafbseg.flcachesize` The maximum cache size in bytes. When the
  storage space exceeds this results are pruned using a LRU algorithm.
  Defaults to `1.5 * 1024^3` when unset.

Note that the default configuration means that the cache will be shared
for a given user across R sessions. It is worth bearing in mind the
possibility of race conditions if multiple applications are
writing/pruning the cache. For example if the `fafbseg.flcachesize` has
different values in different sessions, the session with the smallest
value will start pruning files on disk before the other session.

## See also

Other flywire-ids:
[`flywire_islatest()`](https://natverse.org/fafbseg/reference/flywire_islatest.md),
[`flywire_last_modified()`](https://natverse.org/fafbseg/reference/flywire_last_modified.md),
[`flywire_latestid()`](https://natverse.org/fafbseg/reference/flywire_latestid.md),
[`flywire_rootid()`](https://natverse.org/fafbseg/reference/flywire_rootid.md),
[`flywire_updateids()`](https://natverse.org/fafbseg/reference/flywire_updateids.md),
[`flywire_xyz2id()`](https://natverse.org/fafbseg/reference/flywire_xyz2id.md)

## Examples

``` r
# \donttest{
kcid="720575940623755722"
length(flywire_leaves(kcid))
#> [1] 8536
# }
if (FALSE) { # \dontrun{
# developer function to check cache status
fafbseg:::flywire_leaves_cache_info()
} # }
```
