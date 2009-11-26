\name{nwsIsRunning}
\docType{methods}
\alias{nwsIsRunning}
\alias{nwsIsRunning}
\alias{nwsIsRunning,managedServerInfo-method}
\title{Check if Managed Server Is Running}
\description{
  Check if the server represented by a managedServerInfo object is running
}

\usage{
\S4method{nwsIsRunning}{managedServerInfo}(.Object)
}

\arguments{
  \item{.Object}{a \code{managedServerInfo} class object}
}

\value{
  \code{TRUE} if the server is running, else \code{FALSE}
}

\examples{
\dontrun{
nwsIsRunning(managedServerInfo())
}
}


\keyword{methods}
