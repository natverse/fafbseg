# Return level 2 supervoxel ids for neurons

Level 2 supervoxel ids are one step up from the terminal supervoxel ids
(level 1) that are the finest resolution in the chunked graph. Like root
ids, level 2 supervoxels can be edited: this results in the level 2 id
being destroyed and two new ones being created. Unlike the root id after
a merge or split, only the l2 ids at the edit location change, while
most of the others remain the same.

## Usage

``` r
flywire_l2ids(x, integer64 = TRUE, cache = TRUE)
```

## Arguments

- x:

  root ids including as neuroglancer scene URLs

- integer64:

  Whether to return ids as integer64 type (more compact but a little
  fragile) rather than character (default `FALSE`).

- cache:

  Whether to cache the results on disk

## Value

A vector of ids (usually as 64 bit integers); a named list of vectors
when x has length \>1.

## Examples

``` r
if (FALSE) { # \dontrun{
length(flywire_l2ids("720575940604351334"))
} # }
```
