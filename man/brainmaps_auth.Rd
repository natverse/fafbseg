% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brainmaps-api.R
\name{brainmaps_auth}
\alias{brainmaps_auth}
\title{Generate a token to enable access to brainmaps API}
\usage{
brainmaps_auth(
  client_id = Sys.getenv("BRAINMAPS_CLIENT_ID"),
  client_secret = Sys.getenv("BRAINMAPS_CLIENT_SECRET"),
  scope = "https://www.googleapis.com/auth/brainmaps"
)
}
\arguments{
\item{client_id, client_secret}{Client id and secret copied from
\url{https://console.developers.google.com}. See details.}

\item{scope}{The scope for the brainmaps API - you shouldn't need to change
this.}
}
\value{
A token that can be used with e.g. \code{\link[httr]{GET}}.
}
\description{
Generate a token to enable access to brainmaps API
}
\details{
You will need to activate the brainmaps API, generate a project at
  \url{https://console.developers.google.com} and then request OAuth
  credentials, which will create a client id and client secret. The
  application name on the console must be \code{fafbseg}. You should do this
  once and then put the keys in your \code{\link{.Renviron}} file.

  \itemize{

  \item{\code{BRAINMAPS_CLIENT_ID="xxxx"}}

  \item{\code{BRAINMAPS_CLIENT_SECRET="xxxx"}}

  }
}
\examples{
\dontrun{
google_token=brainmaps_auth()
# get a list of available volumes
req <- GET("https://brainmaps.googleapis.com/v1beta2/volumes",
  config(token = google_token))
}
}
\seealso{
See \code{\link{.Renviron}} for how to set environment variables
}
