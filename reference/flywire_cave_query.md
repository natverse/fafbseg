# Query the FlyWire CAVE annotation system

Query the FlyWire CAVE annotation system

## Usage

``` r
flywire_cave_query(
  table,
  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
  version = NULL,
  timestamp = NULL,
  live = is.null(version),
  timetravel = FALSE,
  filter_in_dict = NULL,
  filter_out_dict = NULL,
  filter_regex_dict = NULL,
  select_columns = NULL,
  offset = 0L,
  limit = NULL,
  fetch_all_rows = FALSE,
  ...
)
```

## Arguments

- table:

  The name of the table (or view, see views section) to query

- datastack_name:

  defaults to the value selected by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  and to "flywire_fafb_production" when that is missing. See
  <https://global.daf-apis.com/info/> for other options.

- version:

  An optional CAVE materialisation version number. See details and
  examples.

- timestamp:

  An optional timestamp as a string or POSIXct, interpreted as UTC when
  no timezone is specified.

- live:

  Whether to use live query mode, which updates any root ids to their
  current value (or to another `timestamp` when provided). Values of
  `TRUE` or `1` select CAVE's *Live* mode, while `2` selects `Live live`
  mode which gives access even to annotations that are not part of a
  materialisation version. See section **Live and Live Live queries**
  for details.

- timetravel:

  Whether to interpret `version`/`timestamp` as a defined point in the
  past to which the very *latest* annotations will be sent back in time,
  recalculating root ids as necessary.

- filter_in_dict, filter_out_dict, filter_regex_dict:

  Optional arguments consisting of key value lists that restrict the
  returned rows (keeping only matches or filtering out matches).
  Commonly used to selected rows for specific neurons. See examples and
  CAVE documentation for details.

- select_columns:

  Either a character vector naming columns or a python dict (required if
  the query involves multiple tables).

- offset:

  a 0-indexed row number, allows you to page through long results (but
  see section **CAVE Row Limits** for some caveats)

- limit:

  whether to limit the number of rows per query (`NULL` implies no
  client side limit but there is typically a server side limit of
  500,000 rows).

- fetch_all_rows:

  Whether to fetch all rows of a query that exceeds limit (default
  `FALSE`). See section **CAVE Row Limits** for some caveats.

- ...:

  Additional arguments to the query method. See examples and details.

## Value

A `tibble`. Note that xyzmatrix can be used on single columns containing
XYZ locations.

## Details

CAVE (Connectome Annotation Versioning Engine) provides a shared
infrastructure for a number of connectomics projects involving Sebastian
Seung's group at Princeton and collaborators at the Allen Institute.
There is both a backend system running on their servers and a Python
client for end users.

You can find out more at <https://caveclient.readthedocs.io/> as well as
looking at the Python notebooks on the github repo
<https://github.com/seung-lab/CAVEclient>.

The annotation system shares authentication infrastructure with the rest
of the FlyWire API (see
[`flywire_set_token`](https://natverse.org/fafbseg/reference/flywire_set_token.md)).

## CAVE Materialisation Versions and Timestamps

CAVE has a concept of table snapshots identified by an integer
materialization `version` number. In some cases you may wish to query a
table at this defined version number so that you can avoid root_ids
changing during an analysis. Your calls will also be faster since no
root id updates are required.

Note however that materialisation versions expire at which point the
corresponding version of the database is expunged. However it is still
possible to find the timestamp for an expired materialisation version.
`flywire_cave_query` does this automatically using
[`flywire_timestamp`](https://natverse.org/fafbseg/reference/flywire_timestamp.md).
In these circumstances queries will again be slower (quite possibly
slower than the live query) since all root ids must be recalculated to
match the timestamp.

CAVE's ability to handle different timepoints is key to analysis for a
continually evolving segmentation but is frankly a little difficult for
users to work with. You will find things simplest if you either

- use a long-term support (LTS) version, which will not expire such as
  the 630 version for the June 2023 public release or the 783 version to
  accompany the published FlyWire papers.

- use the latest CAVE version

## Live and Live Live queries

CAVE versions the segmentation and annotation tables with shared version
numbers. When a dataset is stable, using this single version works well.
However, we have found that this arrangement is not ideal in many
situations, as annotations often evolve even though the segmentation is
static.

CAVE has two options to update annotations to a different point in time:

1.  **Live queries** take the contents of a table at some version and
    updates the root ids to match a later timepoint (usually now).
    However the set of annotations remains stuck at the selected
    version. The starting materialisation version is chosen to be the
    most recent one preceding the requested timepoint.

2.  **Live live queries** (CAVE terminology, `live=2`) can return the
    state of an annotation table at an arbitrary timepoint. This
    includes annotations that have not yet been incorporated into a
    released materialisation version.

We have found that these different query modes do not cover all of our
use cases. In particular we often want to be able to travel *backwards*
in time, taking a current annotation table and mapping the root_ids onto
an earlier state of the segmentation. You can access this functionality
by setting the `timetravel` argument. Under the hood this uses a live
live query using a now timestamp, followed by translating all root ids
back to their earlier state using the associated supervoxel ids.

## CAVE Views

In addition to regular database tables, CAVE provides support for
**views**. These are based on a SQL query which typically aggregates or
combines multiple tables. For an example an aggregation might define the
total number of output synapses for some selected neurons.

At present there are several restrictions on views. For example, you can
only fetch views using an unexpired materialisation version (and you
cannot specify a timepoint using a timestamp) . Furthermore some
`filter_in, filter_out` queries using columns created by the SQL
statement may not be possible.

## CAVE Row Limits

CAVE servers limit the number of rows that can be returned for any
query, typically 500,000 rows. For many queries you can still use
increasing values of `offset` to page through the results. However,
there appear to be restrictions to this. In particular for the synapse
table, rows are returned in *random* order. Therefore, even if you set
your own row `limit` lower than the server's, you still cannot fetch all
the rows of your query. Other tables (e.g. nuclei) do not have this
limitation.

## See also

[`flywire_cave_client`](https://natverse.org/fafbseg/reference/flywire_cave_client.md)

Other cave-queries:
[`flywire_timestamp()`](https://natverse.org/fafbseg/reference/flywire_timestamp.md)

## Examples

``` r
# \donttest{
# note use of limit to restrict the number of rows (must be integer)
n10=flywire_cave_query(table = 'nuclei_v1', limit=10L)
head(as.data.frame(n10))
#>     id             created superceded_id valid     volume pt_supervoxel_id
#> 1 2338 2021-06-23 19:56:18            NA  TRUE  0.2616115                0
#> 2 3078 2021-06-23 19:55:39            NA  TRUE  4.1247950                0
#> 3 3292 2021-06-23 19:56:19            NA  TRUE  0.2737357                0
#> 4 3502 2021-06-23 19:55:39            NA  TRUE  6.7878504                0
#> 5 4404 2021-06-23 19:55:51            NA  TRUE  0.8473395                0
#> 6 4446 2021-06-23 19:55:50            NA  TRUE 58.8006210                0
#>   pt_root_id           pt_position     bb_start_position       bb_end_position
#> 1          0 95520, 254368, 218600 94528, 253184, 218520 96384, 255776, 218680
#> 2          0 97472, 252352, 218040 96128, 248992, 217640 98944, 254720, 218600
#> 3          0 97280, 250784, 219320 96544, 249408, 219240 98080, 251744, 219520
#> 4          0 98304, 253376, 216480 96928, 250752, 215240 99936, 256000, 217240
#> 5          0 81280, 295968, 186680 80640, 294496, 186320 82048, 297600, 187440
#> 6          0 84128, 291584, 193400 81504, 288000, 190240 87104, 295904, 196280
# }
if (FALSE) { # \dontrun{
nuclei_v1=flywire_cave_query(table = 'nuclei_v1')
points3d(xyzmatrix(nuclei_v1$pt_position))

library(elmr)
# calculate signed distance to FAFB surface
# NB this surface is not a perfect fit in the optic lobes
nuclei_v1$d=pointsinside(xyzmatrix(nuclei_v1$pt_position), FAFB.surf,
  rval = 'dist')
points3d(xyzmatrix(nuclei_v1$pt_position),
  col=matlab::jet.colors(20)[cut(nuclei_v1$d,20)])
plot3d(FAFB)
} # }
# Example of a query on a table
if (FALSE) { # \dontrun{
# the Princeton (mostly) and Cambridge groups have tagged some bodies as
# not a neuron - these are often glia.
nans=flywire_cave_query('neuron_information_v2',
  filter_in_dict = list(tag='not a neuron'))
nrow(nans)
table(nans$user_id)
} # }
if (FALSE) { # \dontrun{
psp_351=flywire_cave_query(table = 'proofreading_status_public_v1',
  version=351)
# get the last listed materialisation version
fcc=flywire_cave_client()
lastv=tail(fcc$materialize$get_versions(), n=1)
# pull that
psp_last=flywire_cave_query(table = 'proofreading_status_public_v1',
  version=lastv)
} # }

if (FALSE) { # \dontrun{
# timetravel query example
# note use of allow_missing_lookups=T as not infrequently materialisation
# can fail for a neuron
cambridge_celltypes_v2.783 <- flywire_cave_query('cambridge_celltypes_v2', version = 783,
  timetravel=TRUE, allow_missing_lookups=TRUE,
  select_columns = list(cambridge_celltypes_v2=c("id", "tag", "pt_root_id", "pt_supervoxel_id")))

# querying by regex on a cell type in this table
mbon012<- flywire_cave_query('cambridge_celltypes_v2', version = 783,
timetravel=TRUE, allow_missing_lookups=TRUE, filter_regex_dict = c(tag='MBON0[12]'))
} # }
```
