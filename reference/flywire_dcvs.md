# Get predicted dense core vesicle locations in the flywire dataset

Preliminary dense core vesicle (DCV) detection results from the Lee
group, Stephan Gerhard and Minsu Kim.

## Usage

``` r
flywire_dcvs(
  rootid,
  dataset = c("dcv.3.0", "dcv.2.0", "dcv.1.0"),
  project = "fruitfly_fafb_flywire",
  islatest = TRUE,
  simplify = TRUE,
  return = c("parsed", "text", "response"),
  token = NULL,
  simplifyVector = TRUE,
  include_headers = FALSE,
  OmitFailures = TRUE,
  cl = NULL,
  ...
)
```

## Arguments

- rootid:

  flywire rootid/rootids

- dataset:

  which DCV data set from Stephan Gerhard to access. 1.0 is just for the
  antennal lobes of FAFB. 2.0 is the first run at the whole brain.

- project:

  from which project to pull data. At the moment, there is only one
  option for DCV data.

- islatest:

  logical, whether or not to fetch the latest root_id if the given IDs
  are out of date.

- simplify:

  logical, if `FALSE` each rootid is a separate set of two data frames
  (one for DCV positions, one for the neuron's synapses). Else, a list
  of two combined data frames is returned.

- return:

  One of "parsed", "text" (for raw JSON), or "response"

- token:

  Optional chunkedgraph token (otherwise the default one for the current
  segmentation will be used). Use `NA` to suppress use of a token.

- simplifyVector:

  Whether to use `jsonlite::simplifyVector`

- include_headers:

  Whether to include basic headers from the http request as attributes
  on the parsed JSON object (default `TRUE`) when `parse.json=TRUE`.

- OmitFailures:

  logical, if `TRUE` then requests that result in a 500 error are
  dropped and a warning is displayed but not an error.

- cl:

  A cluster object created by
  [`parallel::makeCluster`](https://rdrr.io/r/parallel/makeCluster.html),
  or an integer to indicate number of child-processes (integer values
  are ignored on Windows) for parallel evaluations.

- ...:

  additional arguments passed to the `httr::{RETRY}` function. This may
  include a [`config`](https://httr.r-lib.org/reference/config.html)
  list other named parameters etc.

## Value

A list, where the first entry contains DCV locations and the second the
synapses for the given rootid, with the nearest DCV precalculated for
each synapse..

## See also

[`braincircuits_login`](https://natverse.org/fafbseg/reference/braincircuits_login.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Just AL test data set
data = flywire_dcvs("720575940629166904", dataset = "dcv.1.0")
dcv = data$dcv
synapse = data$syns

# Whole brain data set
dcv = flywire_dcvs(c("720575940631973089","720575940629166904"), dataset = "dcv.2.0")
} # }
```
