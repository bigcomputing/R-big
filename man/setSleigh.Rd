\name{setSleigh}
\alias{setSleigh}
\title{Set Sleigh object}
\description{
This function registers the specified sleigh object.
It can be accessed later using the \code{getSleigh} function.
}
\note{
This function is useful in conjunction with packages that get their
default sleigh objects using the \code{getSleigh} function.
It allows users to create special sleigh objects that will be
used by any packages that get sleigh objects using the
\code{getSleigh} function.
}
\seealso{\code{\link{getSleigh}}, \code{\link{sleigh}}}

\examples{
\dontrun{
setSleigh(sleigh(workerCount=2))
s <- getSleigh()
eachWorker(s, function() SleighRank)
}
}

\keyword{utilities}
