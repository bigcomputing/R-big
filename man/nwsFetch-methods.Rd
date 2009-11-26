\name{nwsFetch}
\docType{methods}
\alias{nwsFetch}
\alias{nwsFetch-methods}
\alias{nwsFetch,netWorkSpace-method}
\title{Fetch a Stored Value}
\description{
Fetch value associated with a variable from the shared netWorkSpace.
}

\usage{
\S4method{nwsFetch}{netWorkSpace}(.Object, xName)
}


\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of the variable to be fetched}
}

\details{
The \code{nwsFetch} method looks in the shared netWorkSpace \code{.Object} 
for a value bound to \code{xName}; if it finds such a value, \code{nwsFetch} 
returns it and removes it from the variable. If no value is found, \code{nwsFetch} blocks until a value for \code{xName} becomes available.
This operation is atomic. If there are multiple \code{nwsFetch} or 
\code{nwsFetchTry} requests for a given \code{xName}, any given value from 
the set of values associated with \code{xName} will be returned to just one requester. 
If there is more than one value associated with \code{xName}, the particular
value removed depends on \code{xName}'s behavior. See \code{\link{nwsDeclare}} for details.
}
\seealso{ \code{\link{nwsDeclare}}, \code{\link{nwsFetchTry}} }

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 10)
nwsFetch(ws, 'x')
nwsFetch(ws, 'x') # no value for x; therefore block on fetch
}
}

\keyword{methods}
