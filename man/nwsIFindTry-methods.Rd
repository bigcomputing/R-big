\name{nwsIFindTry}
\docType{methods}
\alias{nwsIFindTry}
\alias{nwsIFindTry-methods}
\alias{nwsIFindTry,netWorkSpace-method}
\title{Iterate Through Stored Values of a netWorkSpace Variable}
\description{
Create a function that acts as a non-destructive iterator over the
values of the specified variable.
}

\usage{
\S4method{nwsIFindTry}{netWorkSpace}(.Object, xName, defaultVal=NULL)
}

\arguments{
\item{.Object}{a netWorkSpace class object.}
\item{xName}{character string specifying the name of the variable to be fetched.}
\item{defaultVal}{value to return if \code{xName} is not found in the netWorkSpace.}
}

\details{
The iterator function returned by the \code{nwsIFindTry} method takes
no arguments, and works somewhat like the \code{nwsFindTry} method, specified
with the same arguments that were passed to \code{nwsIFindTry}.
The difference is that the \code{nwsFindTry} method cannot iterate through the
values of a variable; it always returns the same value until the
variable is modified.
The iterator function, however, maintains some state that allows
it to see subsequent values.
Each time the iterator function is called, it returns the next
value in the variable.
Once all values in the variable have been returned, the iterator
function returns \code{defaultVal}.
However, when new values are stored into the variable, the iterator
function will return them, picking right up where it left off.
}
\seealso{ \code{\link{nwsFindTry}}, \code{\link{nwsIFind}} }

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 1)
nwsStore(ws, 'x', 2)
it <- nwsIFindTry(ws, 'x', NA)
it()  # returns the value 1
it()  # returns the value 2
it()  # returns the value NA
nwsStore(ws, 'x', 3)
it()  # returns the value 3
it()  # returns the value NA
}
}

\keyword{methods}
