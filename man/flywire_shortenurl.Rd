% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flywire-urls.R
\name{flywire_shortenurl}
\alias{flywire_shortenurl}
\alias{flywire_expandurl}
\title{Shorten or expand neuroglancer URLs}
\usage{
flywire_shortenurl(x, include_base = TRUE, baseurl = NULL, cache = TRUE, ...)

flywire_expandurl(x, json.only = FALSE, cache = TRUE, ...)
}
\arguments{
\item{x}{One or more neuroglancer URLs or (for flywire_expandurl)
\code{\link{ngscene}} parsed scene description.}

\item{include_base}{Whether to return a full URL that will open a
neuroglancer session (the default) or only the component that defines the
scene (which would display JSON in your browser).}

\item{baseurl}{Optional URL defining the neuroglancer browser to use with
shortened URLs.}

\item{cache}{Whether to cache any calls to the flywire state server
shortening or expanding URLs. Default is \code{TRUE}. NB this cache is only
active for the current session.}

\item{...}{Additional arguments passed to \code{\link{pbsapply}} (when
multiple URLs to process) and then to \code{\link{ngl_encode_url}} (when
generating a short URL for an \code{ngscene} list object) \emph{or} to
\code{flywire_fetch} when using \code{flywire_expandurl}.}

\item{json.only}{Only return the JSON fragment rather than the neuroglancer
URL}
}
\value{
A character vector containing one or more URLs.
}
\description{
\code{flywire_shortenurl} makes short URLs from long URLs or
  \code{\link{ngscene}} objects that you may have constructed in R.

\code{flywire_expandurl} expands shortened URLs into a full
  neuroglancer JSON scene specification. If the active segmentation
  (\code{\link{choose_segmentation}}) is a flywire segmentation then that is
  used to define the initial part of the output URL, otherwise the
  \code{flywire31} segmentation is used.

  \code{flywire_expandurl} will also expand tinyurl.com URLs.
}
\examples{
\dontrun{
sc=ngl_blank_scene()
short=flywire_shortenurl(sc)
long=flywire_expandurl(short)
}
\donttest{
flywire_expandurl("https://globalv1.flywire-daf.com/nglstate/5747205470158848")
flywire_expandurl("https://tinyurl.com/rmr58jpn")
}
}
