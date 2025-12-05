# Print information about fafbseg setup including tokens and python modules

Print information about your **fafbseg** setup including your
FlyWire/ChunkedGraph authentication tokens, Python modules and the
nat.h5reg / java setup required for transforming points between EM and
light level template brains.

## Usage

``` r
dr_fafbseg(pypkgs = NULL)
```

## Arguments

- pypkgs:

  Additional python packages to check beyond the standard ones that
  **fafbseg** knows about such as `cloudvolume`. When set to `FALSE`,
  this turns off the Python package report altogether.

## Examples

``` r
if (FALSE) { # \dontrun{
dr_fafbseg(pymodules=FALSE)
} # }
```
