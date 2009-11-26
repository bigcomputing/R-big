\name{nwsWebPort}
\docType{methods}
\alias{nwsWebPort}
\alias{nwsWebPort-methods}
\alias{nwsWebPort,nwsServer-method}
\alias{nwsWebPort,managedServerInfo-method}
\title{Get web port}
\description{
Get the web port of the NWS server associated with nwsServer
or managedServerInfo object.
}

\usage{
\S4method{nwsWebPort}{nwsServer}(.Object)
\S4method{nwsWebPort}{managedServerInfo}(.Object)
}

\arguments{
  \item{.Object}{an object of class \code{nwsServer} or \code{managedServerInfo}.}
}

\value{
  a double representing the web port associated with the object.
}

\examples{
\dontrun{
# create an nwsServer and get it's web port
si <- managedServerInfo()
nwsWebPort(si)

# create a connection to the server and verify that you get the same web port
nwsWebPort(nwsServer(serverInfo=si))
}
}

\keyword{methods}
