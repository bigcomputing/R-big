\name{nwsPort}
\docType{methods}
\alias{nwsPort}
\alias{nwsPort-methods}
\alias{nwsPort,serverInfo-method}
\title{get port}
\description{
  Get port associated with serverInfo object.
}

\usage{
\S4method{nwsPort}{serverInfo}(.Object)
}

\arguments{
  \item{.Object}{an \code{serverInfo} class or \code{managedServerInfo} class object}
}

\value{
  a double representing the port associated with the serverInfo object
}

\examples{
\dontrun{
# example 
nwsPort(serverInfo())
}
}


\keyword{methods}
