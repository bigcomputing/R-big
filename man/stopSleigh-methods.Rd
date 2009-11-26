\name{stopSleigh}
\docType{methods}
\alias{stopSleigh}
\alias{stopSleigh,sleigh-method}
\title{Stop a Sleigh}
\description{
Shut down sleigh workers and delete sleigh workspace.
}

\usage{
\S4method{stopSleigh}{sleigh}(.Object)
}

\arguments{
\item{.Object}{a sleigh class object}
}

\details{
This function is called by \code{\link{close}} method. 
}

\seealso{\code{\link{close}}}


\examples{
\dontrun{
s = sleigh()
stopSleigh(s)
}
}

\keyword{methods}
