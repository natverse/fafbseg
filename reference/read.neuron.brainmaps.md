# Read skeleton(s) from brainmaps API into a `nat::neuron` object

Read skeleton(s) from brainmaps API into a
[`nat::neuron`](https://rdrr.io/pkg/nat/man/neuron.html) object

## Usage

``` r
read.neuron.brainmaps(x, ...)

read.neurons.brainmaps(x, OmitFailures = NA, df = NULL, ...)
```

## Arguments

- x:

  A vector of segment ids or any Neuroglancer scene specification that
  includes segments ids (see examples and
  [`ngl_segments`](https://natverse.org/fafbseg/reference/ngl_segments.md)
  for details).

- ...:

  Additional arguments passed to
  [`brainmaps_skeleton`](https://natverse.org/fafbseg/reference/brainmaps_skeleton.md)
  and then on to
  [`brainmaps_fetch`](https://natverse.org/fafbseg/reference/brainmaps_fetch.md).
  These may include a `skeletonuri` argument, a brainmaps URI specifying
  the remote source of the skeletons. See
  [`brainmaps_skeleton`](https://natverse.org/fafbseg/reference/brainmaps_skeleton.md)
  for details.

- OmitFailures:

  Whether to omit neurons for which `FUN` gives an error. The default
  value (`NA`) will result in
  [`nlapply`](https://rdrr.io/pkg/nat/man/nlapply.html) stopping with an
  error message the moment there is an error. For other values, see
  details.

- df:

  Optional data frame containing information about each neuron

## Value

a `nat::`[`neuron`](https://rdrr.io/pkg/nat/man/neuron.html) object

## Details

When `OmitFailures` is not `NA`, `FUN` will be wrapped in a call to
[`try`](https://rdrr.io/r/base/try.html) to ensure that failure for any
single neuron does not abort the
[`nlapply`](https://rdrr.io/pkg/nat/man/nlapply.html) call. When
`OmitFailures=TRUE` the resultant
[`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) will be
subsetted down to return values for which `FUN` evaluated successfully.
When `OmitFailures=FALSE`, "try-error" objects will be left in place. In
either of the last 2 cases error messages will not be printed because
the call is wrapped as `try(expr, silent=TRUE)`.

The optional dataframe (`df`) detailing each neuron should have
`rownames` that match the names of each neuron. It would also make sense
if the same key was present in a column of the data frame. If the
dataframe contains more rows than neurons, the superfluous rows are
dropped with a warning. If the dataframe is missing rows for some
neurons an error is generated. If `SortOnUpdate=TRUE` then updating an
existing [`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html)
should result in a new
[`neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html) with
ordering identical to reading all neurons from scratch.

## Examples

``` r
if (FALSE) { # \dontrun{
n=read.neuron.brainmaps(22427007374)
nm=read.neurons.brainmaps(find_merged_segments(7186840767))

# you would specify a particular skeleton source like so
read.neurons.brainmaps(find_merged_segments(7186840767),
   skeletonuri="brainmaps://<volume>/<meshName>")
} # }
```
