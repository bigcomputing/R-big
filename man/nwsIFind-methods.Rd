\name{nwsIFind}
\docType{methods}
\alias{nwsIFind}
\alias{nwsIFind-methods}
\alias{nwsIFind,netWorkSpace-method}
\title{Iterate Through Stored Values of a netWorkSpace Variable}
\description{
Create a function that acts as a non-destructive iterator over the
values of the specified variable.
}

\usage{
\S4method{nwsIFind}{netWorkSpace}(.Object, xName)
}

\arguments{
\item{.Object}{a netWorkSpace class object.}
\item{xName}{character string specifying the name of the variable to be fetched.}
}

\details{
The iterator function returned by the \code{nwsIFind} method takes
no arguments, and works somewhat like the \code{nwsFind} method, specified
with the same arguments that were passed to \code{nwsIFind}.
The difference is that the \code{nwsFind} method cannot iterate through the
values of a variable; it always returns the same value until the
variable is modified.
The iterator function, however, maintains some state that allows
it to see subsequent values.
Each time the iterator function is called, it returns the next
value in the variable.
Once all values in the variable have been returned, the iterator
function blocks, waiting for a new value to be stored in the
variable.
}
\seealso{ \code{\link{nwsFind}}, \code{\link{nwsIFindTry}} }

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 1)
nwsStore(ws, 'x', 2)
it <- nwsIFind(ws, 'x')
it()  # returns the value 1
it()  # returns the value 2
it()  # blocks until another process stores a value in the variable
}
}

\keyword{methods}
