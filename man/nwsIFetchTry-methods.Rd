\name{nwsIFetchTry}
\docType{methods}
\alias{nwsIFetchTry}
\alias{nwsIFetchTry-methods}
\alias{nwsIFetchTry,netWorkSpace-method}
\title{Iterate Through Values of a netWorkSpace Variable}
\description{
Create a function that acts as a destructive iterator over the
values of the specified variable.
}

\usage{
\S4method{nwsIFetchTry}{netWorkSpace}(.Object, xName, defaultVal=NULL)
}

\arguments{
\item{.Object}{a netWorkSpace class object.}
\item{xName}{character string specifying name of the variable to be fetched.}
\item{defaultVal}{value to return, if xName is not found in the netWorkSpace.}
}

\details{
The iterator function returned by the \code{nwsIFetchTry} method takes
no arguments, and works just like the \code{nwsFetchTry} method, specified
with the same arguments that were passed to \code{nwsIFetchTry} method.
Note that the \code{nwsIFind} and \code{nwsIFindTry} methods are much more useful,
since they provide the only way to iterate over values of a variable
non-destructively.
The \code{nwsIFetch} and \code{nwsIFetchTry} methods are provided for completeness.
}
\seealso{ \code{\link{nwsFetchTry}}, \code{\link{nwsIFetch}} }

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 10)
it <- nwsIFetchTry(ws, 'x', NA)
it()  # returns the value 10
it()  # returns NA
}
}

\keyword{methods}
