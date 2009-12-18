\name{nwsFind}
\docType{methods}
\alias{nwsFind}
\alias{nwsFind-methods}
\alias{nwsFind,netWorkSpace-method}
\title{Find a Stored Value}
\description{
Find a value associated with a variable in a shared netWorkSpace.
}

\usage{
\S4method{nwsFind}{netWorkSpace}(.Object, xName)
}

\arguments{
\item{.Object}{a netWorkSpace class object.}
\item{xName}{character string specifying the name of the variable to find.}
}

\details{
The \code{nwsFind} method looks in the shared netWorkSpace \code{.Object}
for a value bound to \code{xName}; if it finds such a value, \code{nwsFind}
returns it but does not remove it. If no value is found, \code{nwsFind}
blocks until a value for \code{xName} becomes available.
If there is more than one value associated with \code{xName}, the
particular value returned depends on \code{xName}'s behavior. See
\code{\link{nwsDeclare}} for details.
}

\seealso{ \code{\link{nwsDeclare}}, \code{\link{nwsFetch}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 1)
x <- nwsFind(ws, 'x')
}
}


\keyword{methods}
