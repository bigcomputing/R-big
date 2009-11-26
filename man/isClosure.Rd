\name{isClosure}
\alias{isClosure}
\title{Determine if a Worker Function is a Closure}
\description{
This is a heuristic function that is used by \code{eachWorker} and 
\code{eachElem} to guess if the worker function is a closure.
}

\details{
If the \code{closure} option wasn't specified via the \code{eo} argument to
\code{eachWorker} or \code{eachElem}, then this function is used to guess if
the worker function is a closure.  It can be very useful to use
closures with \code{eachWorker} and \code{eachElem}, but if not used properly,
you could accidentally include a lot of unnecessary data in the tasks,
thus hurting your performance.

This function is included as a development tool so you can manually
test your worker functions.  This could be useful if you are getting
a warning from \code{eachWorker} or \code{eachElem}, and are trying to determine
how to modify the function or set the \code{closure} option.
}

\examples{
# this should return FALSE
isClosure(sqrt)
f <- function(x) function(y) x + y
g <- f(1)
# this should return TRUE
isClosure(g)
}

\keyword{debugging}
\keyword{utilities}
