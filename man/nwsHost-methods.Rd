\name{nwsHost}
\docType{methods}
\alias{nwsHost}
\alias{nwsHost-methods}
\alias{nwsHost,serverInfo-method}
\title{get hostname}
\description{
  Get hostname associated with serverInfo object.
}

\usage{
\S4method{nwsHost}{serverInfo}(.Object)
}

\arguments{
  \item{.Object}{an \code{serverInfo} class or \code{managedServerInfo} class object}
}

\value{
  A character vector representing the hostname associated with the
  serverInfo object.  If the server is a managed server that has been
  bound to any interface, will return \code{0.0.0.0}.
}

\examples{
\dontrun{
# example 
nwsHost(serverInfo())
}
}


\keyword{methods}
