\name{unexport}
\docType{methods}
\alias{unexport}
\alias{unexport-methods}
\alias{unexport,sleigh-method}
\title{sleigh Class Method}
\description{
Remove a variable from the scope of the workers or specified worker.
}

\usage{
\S4method{unexport}{sleigh}(.Object, xName, worker=NULL)
}

\arguments{
\item{.Object}{a sleigh class object.}
\item{xName}{a variable name (given as a quoted string in the function call).}
\item{worker}{integer rank of the worker where the variable is defined.
The rank values range from 0 to workerCount - 1.
By default, the variable is removed from all workers in the sleigh.}
}

\seealso{\code{\link{export}}}

\examples{
\dontrun{
s <- sleigh()
x <- 6
export(s, 'x', x)
unexport(s, 'x')
}
}

\keyword{methods}
