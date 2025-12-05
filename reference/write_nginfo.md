# Read and write neuroglancer annotation info json files

Read and write neuroglancer annotation info json files

## Usage

``` r
write_nginfo(anndf, f, sep = "_")
```

## Arguments

- anndf:

  A data.frame in which the first column contains ids for each neuron
  and additional columns contain annotations that will be joined into a
  single string.

- f:

  Path to a json file. `write_nginfo` will create the enclosing
  directory if necessary.

- sep:

  The separator used to paste multiple columns of annotations together.

## Value

For `read_nginfo` a list containing annotations.For `write_nginfo`, the
path `f` invisibly.

## Details

Note that there is nothing specific to flywire about these two
functions - they could be used for any data source.

## See also

[`fct2nginfo`](https://natverse.org/fafbseg/reference/fct2nginfo.md)

## Examples

``` r
# \donttest{
tf=tempfile(pattern = "info")
df=data.frame(id=c(10000,10002), type=c("DNp01"))
write_nginfo(df, tf)
# }
```
