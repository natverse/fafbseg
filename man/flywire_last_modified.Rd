% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flywire-api.R
\name{flywire_last_modified}
\alias{flywire_last_modified}
\title{Check when a root id was last edited}
\usage{
flywire_last_modified(x, tz = "UTC", cloudvolume.url = NULL, ...)
}
\arguments{
\item{x}{A set of flywire ids}

\item{tz}{A timezone in which to display the modification time; defaults to
UTC (~ the proper name for GMT).}

\item{cloudvolume.url}{The segmentation source URL for cloudvolume. Normally
you can ignore this and rely on the default segmentation chosen by
\code{\link{choose_segmentation}}}

\item{...}{additional arguments passed to the low-level
\code{\link{flywire_fetch}} function}
}
\value{
A vector of \code{\link{POSIXct}} timestamps, by default in the
  standard UTC timezone.
}
\description{
Check when a root id was last edited
}
\details{
The raw information for this call is in seconds since the epoch,
  00:00 on 1 January 1970 UTC. Specifying a \code{tz} argument changes the
  display but not the actual time of the event i.e. Princeton will typically
  display 5h behind Cambridge corresponding to the same physical time.
}
\examples{
\donttest{
flywire_last_modified("720575940639218165")
# Your local time zone
flywire_last_modified("720575940639218165", tz="")
# Cambridge (the original one)
flywire_last_modified("720575940639218165", tz="GB")
# Princeton
flywire_last_modified("720575940639218165", tz="US/Eastern")
}
}
\seealso{
Other flywire-ids: 
\code{\link{flywire_islatest}()},
\code{\link{flywire_latestid}()},
\code{\link{flywire_leaves}()},
\code{\link{flywire_rootid}()},
\code{\link{flywire_updateids}()},
\code{\link{flywire_xyz2id}()}
}
\concept{flywire-ids}
