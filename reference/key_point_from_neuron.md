# Pick the principal branch point of a neuron (useful for annotation tables)

\`key_point_from_neuron()\` picks a good "key" point on an in-memory
neuron — the principal branch point of its skeleton — suitable for
hanging an annotation off (e.g. a point column in an annotation table).

\`flywire_key_point()\` is a convenience wrapper that reads the L2
skeleton for one or more flywire-style root ids and returns the key
point of each.

## Usage

``` r
key_point_from_neuron(n, reroot = TRUE)

flywire_key_point(ids, raw = TRUE, reroot = TRUE, ...)
```

## Arguments

- n:

  A \`neuron\` (typically an L2 skeleton).

- reroot:

  Whether to reroot onto the furthest endpoint before simplifying.

- ids:

  One or more flywire-style root ids (anything accepted by
  \[read_l2skel()\]).

- raw:

  Whether to return points in raw (voxel) space (default) or nm.

- ...:

  Additional arguments passed to \[pbapply::pbsapply()\].

## Value

\`key_point_from_neuron()\` returns a length-3 nm xyz vector.
\`flywire_key_point()\` returns an N x 3 matrix of points (one row per
input id), in raw voxel space unless \`raw=FALSE\`.

## Details

The point sits at the major branch point of the (L2) skeleton. By
default the neuron is first rerooted onto the endpoint furthest from the
current root, so that simplifying to a single branch point is
well-defined (otherwise the longest path from the root may contain no
branch point at all). If no branch point can be found the original root
point is returned as a fallback.

\`flywire_key_point()\` reads each skeleton with \[read_l2skel()\],
whose dataset is selected by the ambient cave/segmentation context; wrap
the call in a helper such as \`with_crant()\` (crantr) or
\`with_aedes()\` (aedes) to target a non-default segmentation.

## See also

\[read_l2skel()\]

## Examples

``` r
if (FALSE) { # \dontrun{
n <- read_l2skel('720575940621039145')[[1]]
key_point_from_neuron(n)

flywire_key_point('720575940621039145')
} # }
```
