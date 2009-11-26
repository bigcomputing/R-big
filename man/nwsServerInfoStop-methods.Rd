\name{nwsServerInfoStop}
\docType{methods}
\alias{nwsServerInfoStop}
\alias{nwsServerInfoStop-methods}
\alias{nwsServerInfoStop,managedServerInfo-method}
\title{Stop the managed server}
\description{
  Stop the server represented by an managedServerInfo object. All
  data on server will be lost and all tasks will be killed.
}

\usage{
\S4method{nwsServerInfoStop}{managedServerInfo}(.Object)
}

\arguments{
  \item{.Object}{a \code{managedServerInfo} class object}
}

\examples{
\dontrun{
# example
nwsServerInfoStop(managedServerInfo())
}
}


\keyword{methods}
