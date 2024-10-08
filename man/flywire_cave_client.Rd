% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cave.R
\name{flywire_cave_client}
\alias{flywire_cave_client}
\title{Low level access to the Flywire CAVE annotation system}
\usage{
flywire_cave_client(
  datastack_name = getOption("fafbseg.cave.datastack_name", "flywire_fafb_production")
)
}
\arguments{
\item{datastack_name}{defaults to the value selected by
\code{\link{choose_segmentation}} and to "flywire_fafb_production" when
that is missing. See \url{https://global.daf-apis.com/info/} for other
options.}
}
\value{
The \code{caveclient.frameworkclient.CAVEclientFull} Python module
  wrapped by reticulate.
}
\description{
Low level access to the Flywire CAVE annotation system
}
\details{
This depends on installation of the Python caveclient library. See
  \code{\link{flywire_cave_query}} for more details.
}
\examples{
\donttest{
fac <- flywire_cave_client()
fac$annotation$get_tables()
fac$annotation$get_table_metadata('nuclei_v1')

# annotation tables that have been materialised in order to map XYZ
# points onto current root ids (and immutable supervoxel ids)
# typically the same as fac$annotation$get_tables()
fac$materialize$get_tables()

info=fac$info$get_datastack_info()
# the default synapse table for the dataset
info$synapse_table
}

\dontrun{
# get help on python cave client
reticulate::py_help(fac$info$get_datastack_info)
}
}
