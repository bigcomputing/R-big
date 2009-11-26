\name{nwsFetchFile}
\docType{methods}
\alias{nwsFetchFile}
\alias{nwsFetchFile-methods}
\alias{nwsFetchFile,netWorkSpace-method}
\title{Fetch a Value and Write It to a File}
\description{
Fetch a value of a netWorkSpace variable and write it to a file.
}

\usage{
\S4method{nwsFetchFile}{netWorkSpace}(.Object, xName, fObj)
}

\arguments{
\item{.Object}{a netWorkSpace object.}
\item{xName}{character string giving the name of the variable to find.}
\item{fObj}{\code{file} object or character string specifying file to write data to.}
}

\details{
The \code{nwsFetchFile} method looks in the shared netWorkSpace \code{.Object}
for a value bound to \code{xName}; if it finds such a value, \code{nwsFetchFile}
writes the value to the specified file and removes it from the variable. If no value is found, \code{nwsFetchFile} blocks until a value for \code{xName} 
becomes available.
}

\seealso{ \code{\link{nwsFindFile}}, \code{\link{nwsFetch}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 'Hello, world\n')
nwsFetchFile(ws, 'x', 'hello.txt')
}
}

\keyword{methods}
