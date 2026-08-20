#' Find a good "key" point on a flywire-style neuron for annotations
#'
#' @description The chosen point sits at the major branch point of the L2
#'   skeleton of each neuron. By default the L2 skeleton is rerooted onto the
#'   endpoint furthest from the current root so that a simplified
#'   representation with one branch point can be calculated; without this, the
#'   longest path from the root may not contain a branch point at all. If no
#'   branch point can be identified the original root point is used as a
#'   fallback.
#'
#' @details Reads an L2 skeleton via [read_l2skel()] for each root id, then
#'   picks the principal branch point with [key_point_from_neuron()]. The
#'   ambient cave/segmentation context selects the dataset, so wrap the call in
#'   a dataset helper such as `with_crant()` (crantr) or `with_aedes()` (aedes)
#'   to target a non-default segmentation.
#'
#' @param ids One or more flywire-style root ids (anything accepted by
#'   [read_l2skel()]).
#' @param raw Whether to return points in raw (voxel) space (default) or nm.
#' @param reroot Whether to reroot the incoming neuron onto the furthest
#'   endpoint before simplifying.
#' @param ... Additional arguments passed to [pbapply::pbsapply()].
#' @return An N x 3 matrix of point locations (one row per input id).
#' @seealso [key_point_from_neuron()], [read_l2skel()]
#' @export
#' @examples
#' \dontrun{
#' flywire_key_point('720575940621039145')
#' }
flywire_key_point <- function(ids, raw = TRUE, reroot = TRUE, ...) {
  if (length(ids) > 1) {
    res <- pbapply::pbsapply(ids, flywire_key_point, raw = raw, reroot = reroot, ...)
    return(t(res))
  }
  tryCatch({
    n <- read_l2skel(ids)[[1]]
    nmpt <- key_point_from_neuron(n, reroot = reroot)
    if (raw) flywire_nm2raw(nmpt) else nmpt
  }, error = function(e) {
    warning("Unable to extract key point for id: ", ids, ": ", conditionMessage(e))
    cbind(NA, NA, NA)
  })
}

#' Pick the principal branch point of a neuron
#'
#' @description Pure helper operating on an in-memory `neuron` (typically an L2
#'   skeleton). Reroots onto the endpoint furthest from the current root (so the
#'   longest path through the neuron passes through at least one branch point),
#'   simplifies to a single branch point, and returns the xyz of that branch
#'   point in nm. Falls back to the original root point with a warning if no
#'   branch point can be found.
#'
#' @param n A `neuron` (typically an L2 skeleton).
#' @param reroot Whether to reroot onto the furthest endpoint first.
#' @return A length-3 nm xyz vector.
#' @seealso [flywire_key_point()]
#' @export
#' @examples
#' \dontrun{
#' n <- read_l2skel('720575940621039145')[[1]]
#' key_point_from_neuron(n)
#' }
key_point_from_neuron <- function(n, reroot = TRUE) {
  if (reroot) {
    eps <- nat::endpoints(n)
    ng <- nat::as.ngraph(n, weights = TRUE)
    # there should only be one rootpoint but just occasionally ...
    epdists <- igraph::distances(ng, v = nat::rootpoints(n)[1], to = eps)
    n <- nat::reroot(n, eps[which.max(epdists)])
  }
  n1 <- nat::simplify_neuron(n, n = 1)
  bp1 <- nat::branchpoints(n1)
  if (length(bp1) < 1) {
    warning("Unable to extract key point, falling back to root!")
    bp1 <- 1L
  }
  nat::xyzmatrix(n1)[bp1[1], ]
}
