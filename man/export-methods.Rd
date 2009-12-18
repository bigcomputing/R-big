\name{export}
\docType{methods}
\alias{export}
\alias{export-methods}
\alias{export,sleigh-method}
\title{sleigh Class Method}
\description{
Define a variable in the scope of the workers or specified worker.
}

\usage{
\S4method{export}{sleigh}(.Object, xName, xVal, worker=NULL)
}

\arguments{
\item{.Object}{a sleigh class object.}
\item{xName}{a variable name (given as a quoted string in the function call).}
\item{xVal}{a value to be assigned to 'x'.}
\item{worker}{integer rank of the worker where the variable is defined.
The rank values range from 0 to workerCount - 1.
By default, the variable is defined on all workers in the sleigh.}
}

\seealso{\code{\link{unexport}}}

\examples{
\dontrun{
s <- sleigh()
m <- matrix(rnorm(16), 4, 4)
export(s, 'm', m)
eachWorker(s, function() m \%*\% matrix(rnorm(16), 4, 4), eo=list(closure=FALSE))
}
}

\keyword{methods}
