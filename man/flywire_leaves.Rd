% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flywire-api.R
\name{flywire_leaves}
\alias{flywire_leaves}
\title{Find all the supervoxel (leaf) ids that are part of a FlyWire object}
\usage{
flywire_leaves(
  x,
  cloudvolume.url = NULL,
  integer64 = FALSE,
  mip = 0L,
  bbox = NULL,
  cache = TRUE,
  ...
)
}
\arguments{
\item{x}{One or more FlyWire segment ids}

\item{cloudvolume.url}{URL for CloudVolume to fetch segmentation image data.
The default value of NULL chooses the flywire production segmentation
dataset.}

\item{integer64}{Whether to return ids as integer64 type (the default, more
compact but a little fragile) rather than character (when \code{FALSE}).}

\item{mip}{The mip level for the segmentation (expert use only)}

\item{bbox}{The bounding box within which to find supervoxels (default =
\code{NULL} for whole brain. Expert use only.)}

\item{cache}{Whether to cache the results of flywire_leaves calls. See
details.}

\item{...}{Additional arguments passed to \code{\link{pbsapply}} and
eventually \code{\link{flywire_fetch}} when \code{method="flywire"} OR to
\code{cv$CloudVolume} when \code{method="cloudvolume"}}
}
\description{
This workhorse function underlies the ability to define synaptic
  connections and neurotransmitter predictions for flywire neurons.
}
\details{
By default repeated calls to \code{flywire_leaves} are cached on
  disk. This functionality is provided by the \code{cachem} package now used
  by the \code{memoise} package. By default the cache will expand up to 1.5GB
  and then start pruning on a least recently used basis (LRU). 1.5GB might
  store 10-20,000 results for \code{flywire_leaves} calls depending on the
  size of the corresponding neurons.

  Since each root id can map to hundreds of thousands of supervoxel ids,
  there are space implications. In order to save space, the results are
  stored as compressed 64 bit integers (which are ~30x smaller than character
  vectors). The compression step does add an extra ~ 5% time on a cache miss
  but is 100x + faster on a cache hit. The default compression is based on
  the suggested brotli library if available, gzip otherwise.

  There is functionality for a memory cache on top of the disk cache, but
  this is not currently exposed as the disk read time appears small compared
  with the time for uncompressing and other overheads.

  The cache can be controlled by two package options:

  \itemize{

  \item \code{fafbseg.cachedir} The location on disk. If not previously set,
  it is set to an appropriate user folder on package load using
  \code{rappdirs::\link[rappdirs]{user_data_dir}}. Note that the cache for
  this function will be located inside a folder called \code{flywire_leaves}.

  \item \code{fafbseg.flcachesize} The maximum cache size in bytes. When the
  storage space exceeds this results are pruned using a LRU algorithm.
  Defaults to \code{1.5 * 1024^3} when unset.

  }

  Note that the default configuration means that the cache will be shared for
  a given user across R sessions. It is worth bearing in mind the possibility
  of race conditions if multiple applications are writing/pruning the cache.
  For example if the \code{fafbseg.flcachesize} has different values in
  different sessions, the session with the smallest value will start pruning
  files on disk before the other session.
}
\examples{
\donttest{
kcid="720575940623755722"
length(flywire_leaves(kcid))
}
\dontrun{
# developer function to check cache status
fafbseg:::flywire_leaves_cache_info()
}
}
\seealso{
Other flywire-ids: 
\code{\link{flywire_islatest}()},
\code{\link{flywire_last_modified}()},
\code{\link{flywire_latestid}()},
\code{\link{flywire_rootid}()},
\code{\link{flywire_updateids}()},
\code{\link{flywire_xyz2id}()}
}
\concept{flywire-ids}
