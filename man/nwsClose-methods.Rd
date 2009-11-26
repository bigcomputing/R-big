\name{nwsClose}
\docType{methods}
\alias{nwsClose}
\alias{nwsClose-methods}
\alias{nwsClose,netWorkSpace-method}
\title{Close a netWorkSpace}
\description{
Close connection of a shared netWorkSpace to the netWorkSpace server.
}

\usage{
\S4method{nwsClose}{netWorkSpace}(.Object)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
# do some works
# ...
nwsClose(ws)
}
}

\keyword{methods}
