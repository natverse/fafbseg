#' Pick the principal branch point of a neuron (useful for annotation tables)
#'
#' @description `key_point_from_neuron()` picks a good "key" point on an
#'   in-memory neuron — the principal branch point of its skeleton — suitable
#'   for hanging an annotation off (e.g. a point column in an annotation table).
#'
#'   `flywire_key_point()` is a convenience wrapper that reads the L2 skeleton
#'   for one or more flywire-style root ids and returns the key point of each.
#'
#' @details The point sits at the major branch point of the (L2) skeleton. By
#'   default the neuron is first rerooted onto the endpoint furthest from the
#'   current root, so that simplifying to a single branch point is well-defined
#'   (otherwise the longest path from the root may contain no branch point at
#'   all). If no branch point can be found the original root point is returned
#'   as a fallback.
#'
#'   `flywire_key_point()` reads each skeleton with [read_l2skel()], whose
#'   dataset is selected by the ambient cave/segmentation context; wrap the call
#'   in a helper such as `with_crant()` (crantr) or `with_aedes()` (aedes) to
#'   target a non-default segmentation.
#'
#' @param n A `neuron` (typically an L2 skeleton).
#' @param reroot Whether to reroot onto the furthest endpoint before
#'   simplifying.
#' @return `key_point_from_neuron()` returns a length-3 nm xyz vector.
#'   `flywire_key_point()` returns an N x 3 matrix of points (one row per input
#'   id), in raw voxel space unless `raw=FALSE`.
#' @seealso [read_l2skel()]
#' @export
#' @examples
#' \dontrun{
#' n <- read_l2skel('720575940621039145')[[1]]
#' key_point_from_neuron(n)
#'
#' flywire_key_point('720575940621039145')
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

#' @param ids One or more flywire-style root ids (anything accepted by
#'   [read_l2skel()]).
#' @param raw Whether to return points in raw (voxel) space (default) or nm.
#' @param ... Additional arguments passed to [pbapply::pbsapply()].
#' @rdname key_point_from_neuron
#' @export
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
