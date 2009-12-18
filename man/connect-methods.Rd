\name{connect}
\docType{methods}
\alias{connect}
\alias{connect-methods}
\alias{connect,sleigh-method}
\title{Create socket connections between the master and each of the sleigh workers.}
\description{
This provides a way to efficiently transfer large objects back and forth
between the master and workers.
}

\usage{
\S4method{connect}{sleigh}(.Object, master, port)
}

\arguments{
\item{.Object}{an object of class \code{sleigh}.}
\item{master}{the address of the master.}
\item{port}{the port number that the worker should connect to.}
}

\examples{
\dontrun{
# create a sleigh and create connections to each of the workers
s = sleigh()
connect(s)
}
}

\keyword{methods}
