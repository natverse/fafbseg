# Query FlyWire level 2 cache attributes

Query FlyWire level 2 cache attributes

## Usage

``` r
flywire_l2attributes(
  rootid = NULL,
  l2ids = NULL,
  attributes = NULL,
  split_columns = TRUE,
  rval = c("data.frame", "list"),
  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production"),
  ...
)
```

## Arguments

- rootid:

  One or more FlyWire root ids defining a segment. Supply either
  `rootid` or `l2ids`.

- l2ids:

  Optional vector of level 2 ids to query directly. Supply either
  `rootid` or `l2ids`.

- attributes:

  Optional character vector of attribute names to retrieve. `NULL`
  requests all available attributes.

- split_columns:

  Whether to expand multivalued attributes into separate columns using
  the same names as python `caveclient`.

- rval:

  Return either a `data.frame` or the raw named `list` returned by
  python `caveclient`.

- datastack_name:

  defaults to the value selected by
  [`choose_segmentation`](https://natverse.org/fafbseg/reference/choose_segmentation.md)
  and to "flywire_fafb_production" when that is missing. See
  <https://global.daf-apis.com/info/> for other options.

- ...:

  Additional arguments passed to
  [`flywire_l2ids`](https://natverse.org/fafbseg/reference/flywire_l2ids.md)
  and to recursive calls when `rootid` contains multiple ids.

## Value

When `rval="data.frame"`, a tibble with one row per queried level 2 id
and an explicit `l2_id` column stored as a 64 bit integer. When
`rval="list"`, the raw named list returned by
`caveclient$l2cache$get_l2data()`. When `rootid` has length greater than
1, a named list is returned with one result per root id.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- flywire_l2attributes(
  rootid = "720575940604351334",
  attributes = c("size_nm3", "rep_coord_nm")
)
head(x)
} # }
```
