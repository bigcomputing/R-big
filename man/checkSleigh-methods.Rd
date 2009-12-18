\name{checkSleigh}
\docType{methods}
\alias{checkSleigh}
\alias{checkSleigh-methods}
\alias{checkSleigh,sleighPending-method}
\title{Number of Results Pending From Sleigh}
\description{
Return the number of results yet to be generated for the pending sleigh job. 
}

\usage{
\S4method{checkSleigh}{sleighPending}(.Object)
}

\arguments{
\item{.Object}{a \code{sleighPending} class object}
}

\details{
The \code{sleighPending} class object, \code{.Object}, is usually 
obtained through non-blocking \code{eachElem} or non-blocking 
\code{eachWorker}. If the pending job is finished, that is, 
all results are generated, then 0 is returned. 
}
\seealso{ \code{\link{eachWorker}}, \code{\link{eachElem}} }

\examples{
\dontrun{
eo = list(blocking=0)
s = sleigh()
sp = eachElem(s, function(x) {Sys.sleep(100); x}, list(1:10), eo=eo) 
checkSleigh(sp)  
}
}

\keyword{methods}

