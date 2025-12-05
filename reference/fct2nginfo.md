# Convert flytable cell type information into a neuroglancer info file

Convert flytable cell type information into a neuroglancer info file

## Usage

``` r
fct2nginfo(f, ids = NULL, version = NULL, sep = "_", gluestr = NULL, ...)
```

## Arguments

- f:

  Path to a json file. `write_nginfo` will create the enclosing
  directory if necessary.

- ids:

  FlyWire root ids in any form understood by
  [`flywire_ids`](https://natverse.org/fafbseg/reference/flywire_ids.md)

- version:

  Optional numeric CAVE version (see `flywire_cave_query`). The special
  signalling value of `TRUE` uses the current default data dump as
  returned by
  [`flywire_connectome_data_version`](https://natverse.org/fafbseg/reference/flywire_connectome_data.md).

- sep:

  The separator used to paste multiple columns of annotations together.

- gluestr:

  Optional string passed to
  [`glue::glue`](https://glue.tidyverse.org/reference/glue.html) which
  is interpreted in the context of the annotation data.frame produced by
  [`flytable_meta`](https://natverse.org/fafbseg/reference/add_celltype_info.md).
  This allows arbitrary formatting for

- ...:

  Additional arguments passed to
  [`flytable_meta`](https://natverse.org/fafbseg/reference/add_celltype_info.md)
  and then eventually
  [`flytable_cell_types`](https://natverse.org/fafbseg/reference/flytable_cell_types.md).

## Value

The path `f` invisibly

## See also

[`write_nginfo`](https://natverse.org/fafbseg/reference/write_nginfo.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# all neurons in info table
fct2nginfo(f='path/to/flytablev526/info', version=526)
fct2nginfo(f='path/to/hemilineagev526/info', version=526,
  gluestr="{ito_lee_hemilineage}_{toupper(substr(side,1,1))}")
fct2nginfo("MBON%", 'path/to/mboninfo/info')
} # }
```
