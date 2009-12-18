\name{nwsFetchTryFile}
\docType{methods}
\alias{nwsFetchTryFile}
\alias{nwsFetchTryFile-methods}
\alias{nwsFetchTryFile,netWorkSpace-method}
\title{Fetch a Stored Value and Write It to a File}
\description{
Fetch a value of a netWorkSpace variable and write it to a file.
}

\usage{
\S4method{nwsFetchTryFile}{netWorkSpace}(.Object, xName, fObj)
}

\arguments{
\item{.Object}{a netWorkSpace object.}
\item{xName}{character string specifying name of the variable to find.}
\item{fObj}{\code{file} object or character string specifying file to write data to.}
}

\details{
The \code{nwsFetchTryFile} method looks in the shared netWorkSpace \code{.Object}
for a value bound to \code{xName};  if it finds such a value, \code{nwsFetchTryFile}
 writes the value to the specified file, removes it from the variable, and the 
method returns \code{TRUE}.  If it is not found,
\code{nwsFetchTryFile} returns FALSE.
}

\seealso{ \code{\link{nwsFindTryFile}}, \code{\link{nwsFetchTry}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 'Hello, world\n')
if (nwsFetchTryFile(ws, 'x', 'hello.txt')) {
  cat('success\n')
} else {
  cat('failure\n')
}
}
}

\keyword{methods}
