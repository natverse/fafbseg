# Get flywire IDs that map onto CATMAID neurons

Provide this function with a CATMAID query (skeleton IDs or an
annotation term, as you could provide to
[`catmaid::catmaid_skids`](https://rdrr.io/pkg/catmaid/man/catmaid_skids.html))
or a [`nat::neuronlist`](https://rdrr.io/pkg/nat/man/neuronlist.html)
object in FAFB14 space, and it will return a ranked list of flywire IDs
that map onto the given neuron(s).

## Usage

``` r
fafb14_to_flywire_ids(
  search,
  only.root = FALSE,
  only.biggest = FALSE,
  pid = 1L,
  conn = NULL,
  fetch.annotations = FALSE,
  OmitFailures = FALSE,
  ...
)
```

## Arguments

- search:

  one or more skids or a CATMAID query expression. Else, a neuronlist of
  neurons in FAFB14 space.

- only.root:

  only return one `root_id` at the location of the root node of the
  given CATMAID neuron(s)..

- only.biggest:

  only return one `root_id` per CATMAID `skid` i.e. the biggest
  overlapping fragment.

- pid:

  Project id (default 1)

- conn:

  A `catmaid_connection` objection returned by
  [`catmaid_login`](https://rdrr.io/pkg/catmaid/man/catmaid_login.html).
  If `NULL` (the default) a new connection object will be generated
  using the values of the **catmaid.\*** package options as described in
  the help for
  [`catmaid_login`](https://rdrr.io/pkg/catmaid/man/catmaid_login.html).

- fetch.annotations:

  Whether or not to fetch the annotations for each skeleton (default
  `FALSE`)

- OmitFailures:

  logical, whether to omit neurons that cannot be read from CATMAID.

- ...:

  Additional arguments passed to
  [`nat::nlapply`](https://rdrr.io/pkg/nat/man/nlapply.html).

## Value

A `data.frame` of ranked flywire IDs, and the number of points in each
CATMAID neuron that maps to that ID.

## Examples

``` r
if (FALSE) { # \dontrun{
# a specific skid
df=fafb14_to_flywire_ids(16)
head(df)

# Get neurons from a specific CATMAID environment
## See catmaid package help for  details on how to 'login'

# This is the Drosophila anatomy ontology identifier for DL1
# adult antennal lobe projection neuron DL1 adPN
# see \url{https://virtualflybrain.org} for details.
hits=fafb14_to_flywire_ids(search="FBbt:00067353", conn=catmaid::vfbcatmaid())
head(hits)
} # }
```
