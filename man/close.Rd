\name{close}
\alias{close}
\alias{close.sleigh}
\alias{close.nwsServer}
\title{Close a Sleigh or nwsServer}
\description{
If the first argument is a \code{sleigh} object, then \code{close} calls 
\code{stopSleigh} to shut down the sleigh workers and delete the sleigh 
workspace. 
If the first argument is a \code{nwsServer} object, then the connection to the
netWorkSpace server is closed.
}

\usage{
\method{close}{sleigh}(con, ...)
\method{close}{nwsServer}(con, ...)
}

\arguments{
\item{con}{an object of class \code{sleigh} or \code{nwsServer}.}
\item{...}{optional fields}
}

\details{
The optional arguments are not passed to \code{stopSleigh} method. 
They are defined to be compatible with the default, non-generic 
\code{close} method.
}

\examples{
\dontrun{
s = sleigh()
close(s)

wss = nwsServer()
close(wss)
}
}

\keyword{methods}
