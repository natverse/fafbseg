# Pick the principal branch point of a neuron

Pure helper operating on an in-memory \`neuron\` (typically an L2
skeleton). Reroots onto the endpoint furthest from the current root (so
the longest path through the neuron passes through at least one branch
point), simplifies to a single branch point, and returns the xyz of that
branch point in nm. Falls back to the original root point with a warning
if no branch point can be found.

## Usage

``` r
key_point_from_neuron(n, reroot = TRUE)
```

## Arguments

- n:

  A \`neuron\` (typically an L2 skeleton).

- reroot:

  Whether to reroot onto the furthest endpoint first.

## Value

A length-3 nm xyz vector.

## See also

\[flywire_key_point()\]

## Examples

``` r
if (FALSE) { # \dontrun{
n <- read_l2skel('720575940621039145')[[1]]
key_point_from_neuron(n)
} # }
```
