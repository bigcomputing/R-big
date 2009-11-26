\name{nwsFindTryFile}
\docType{methods}
\alias{nwsFindTryFile}
\alias{nwsFindTryFile-methods}
\alias{nwsFindTryFile,netWorkSpace-method}
\title{Find a Stored Value and Write It to a File}
\description{
Find a value of a netWorkSpace variable and write it to a file.
}

\usage{
\S4method{nwsFindTryFile}{netWorkSpace}(.Object, xName, fObj)
}

\arguments{
\item{.Object}{a netWorkSpace object.}
\item{xName}{character string specifying the name of the variable to find.}
\item{fObj}{\code{file} object or character string specifying the file to write data to.}
}

\details{
The \code{nwsFindTryFile} method looks in the shared netWorkSpace \code{.Object}
for a value bound to \code{xName}; if it finds such a value, \code{nwsFind}
writes it to the specified file but does not remove it from the variable, and returns \code{TRUE}.
If no value is found, \code{nwsFindTryFile} returns \code{FALSE}.
If there is more than one value associated with \code{xName}, the
particular value returned depends on \code{xName}'s behavior. See
\code{\link{nwsDeclare}} for details.
}

\seealso{ \code{\link{nwsFetchTryFile}}, \code{\link{nwsFindTry}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 'Hello, world\n')
if (nwsFindTryFile(ws, 'x', 'hello.txt')) {
  cat('success\n')
} else {
  cat('failure\n')
}
}
}

\keyword{methods}
