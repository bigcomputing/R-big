\name{nwsServerObject}
\docType{methods}
\alias{nwsServerObject}
\alias{nwsServerObject-methods}
\alias{nwsServerObject,netWorkSpace-method}
\title{Return Server Object Associated with netWorkSpace}
\description{
Return the \code{nwsServer} object associated with a netWorkSpace.
}

\usage{
\S4method{nwsServerObject}{netWorkSpace}(.Object)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
}

\examples{
\dontrun{
ws = netWorkSpace('nws example')
nwsServerObject(ws)
}
}
\keyword{methods}
