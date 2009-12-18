\name{nwsStoreFile}
\docType{methods}
\alias{nwsStoreFile}
\alias{nwsStoreFile-methods}
\alias{nwsStoreFile,netWorkSpace-method}
\title{Read a Value from File and Store in netWorkSpace}
\description{
Store a new value into a variable in the workspace from a file.
}

\usage{
\S4method{nwsStoreFile}{netWorkSpace}(.Object, xName, fObj, n=0)
}

\arguments{
\item{.Object}{a netWorkSpace class object.}
\item{xName}{character string specifying the name of the variable to be stored}
\item{fObj}{a \code{file} object or character string specifying file from which to read data to store in the variable.}
\item{n}{Number of bytes to write.  A value of zero means to
write all the data in the file.}
}

\details{
The \code{nwsStoreFile} method works like \code{nwsStore}, except that the value to
store in the netWorkSpace variable comes from a file.
If \code{fObj} is a character string, \code{nwsStoreFile} calls \code{file}
to obtain a file connection which is opened for the duration
of the method.
}

\seealso{\code{\link{nwsStore}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsStore(ws, 'x', 'Hello, world\n')
nwsFindFile(ws, 'x', 'hello.txt')
nwsStoreFile(ws, 'hello', 'hello.txt')
}
}

\keyword{methods}
