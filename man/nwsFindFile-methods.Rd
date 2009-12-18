\name{nwsFindFile}
\docType{methods}
\alias{nwsFindFile}
\alias{nwsFindFile-methods}
\alias{nwsFindFile,netWorkSpace-method}
\title{Find a Stored Value and Write It to a File}
\description{
Find a value of a workspace variable and write it to a file.
}

\usage{
\S4method{nwsFindFile}{netWorkSpace}(.Object, xName, fObj)
}

\arguments{
\item{.Object}{a netWorkSpace object.}
\item{xName}{character string specifying the name of the variable to find.}
\item{fObj}{\code{file} object or character string specifying the file to write data to.}
}

\details{
The \code{nwsFindFile} method looks in the shared netWorkSpace \code{.Object}
for a value bound to \code{xName}; if it finds such a value, \code{nwsFind}
writes it to the specified file but does not remove it from the variable. 
If no value is found, \code{nwsFindFile}
blocks until a value for \code{xName} becomes available.
If there is more than one value associated with \code{xName}, the
particular value returned depends on \code{xName}'s behavior. See
\code{\link{nwsDeclare}} for details.
}

\seealso{ \code{\link{nwsFetchFile}}, \code{\link{nwsFind}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 'Hello, world\n')
nwsFindFile(ws, 'x', 'hello.txt')
}
}

\keyword{methods}
