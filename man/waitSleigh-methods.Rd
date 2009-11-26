\name{waitSleigh}
\docType{methods}
\alias{waitSleigh}
\alias{waitSleigh-methods}
\alias{waitSleigh,sleighPending-method}
\title{Wait for Results from Sleigh}
\description{
Wait and block for the results to be generated for the pending sleigh job. 
If the job has completed, then \code{waitSleigh} returns immediately with 
the generated results.
}

\usage{
\S4method{waitSleigh}{sleighPending}(.Object)
}

\arguments{
\item{.Object}{a \code{sleighPending} class object}
}

\details{
The \code{sleighPending} class object, \code{.Object}, is usually obtained 
from the return value of non-blocking \code{eachElem} or non-blocking 
\code{eachWorker}. 
}
\seealso{ \code{\link{eachWorker}}, \code{\link{eachElem}} }


\examples{
\dontrun{
s = sleigh()
eo = list(blocking=0)
sp = eachWorker(s, function() {Sys.sleep(100)}, eo=eo) 
waitSleigh(sp)  # wait on workers. Each worker sleeps for 100 seconds
}
}


\keyword{methods}
