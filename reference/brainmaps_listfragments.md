# Low level call to brainmaps API to list mesh fragment ids for segment ids

Low level call to brainmaps API to list mesh fragment ids for segment
ids

## Usage

``` r
brainmaps_listfragments(
  x,
  volume = getOption("fafbseg.brainmaps.volume"),
  meshName = getOption("fafbseg.brainmaps.meshName"),
  ...
)
```

## Arguments

- x:

  Single segment identifier

- volume:

  String identifier for the volume containing segments

- meshName:

  String identifier for the meshes

- ...:

  Additional arguments passed to
  [`brainmaps_fetch`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md)

## Value

Character vector of fragment ids

## See also

[`read_brainmaps_meshes`](https://natverse.org/fafbseg/reference/read_brainmaps_meshes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
brainmaps_listfragments(7186840767)
} # }
```
