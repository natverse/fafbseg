# Find a good "key" point on a flywire-style neuron for annotations

The chosen point sits at the major branch point of the L2 skeleton of
each neuron. By default the L2 skeleton is rerooted onto the endpoint
furthest from the current root so that a simplified representation with
one branch point can be calculated; without this, the longest path from
the root may not contain a branch point at all. If no branch point can
be identified the original root point is used as a fallback.

## Usage

``` r
flywire_key_point(ids, raw = TRUE, reroot = TRUE, ...)
```

## Arguments

- ids:

  One or more flywire-style root ids (anything accepted by
  \[read_l2skel()\]).

- raw:

  Whether to return points in raw (voxel) space (default) or nm.

- reroot:

  Whether to reroot the incoming neuron onto the furthest endpoint
  before simplifying.

- ...:

  Additional arguments passed to \[pbapply::pbsapply()\].

## Value

An N x 3 matrix of point locations (one row per input id).

## Details

Reads an L2 skeleton via \[read_l2skel()\] for each root id, then picks
the principal branch point with \[key_point_from_neuron()\]. The ambient
cave/segmentation context selects the dataset, so wrap the call in a
dataset helper such as \`with_crant()\` (crantr) or \`with_aedes()\`
(aedes) to target a non-default segmentation.

## See also

\[key_point_from_neuron()\], \[read_l2skel()\]

## Examples

``` r
if (FALSE) { # \dontrun{
flywire_key_point('720575940621039145')
} # }
```
