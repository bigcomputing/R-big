\name{rankCount}
\docType{methods}
\alias{rankCount-methods}
\alias{rankCount,sleigh-method}
\alias{rankCount}
\title{Return a Rank Count from a Sleigh}
\description{
Return the \code{rankCount} in sleigh. If \code{rankCount=-1}, then the 
worker group has closed, and no more workers may join the group. 
}

\usage{
\S4method{rankCount}{sleigh}(.Object)
}

\arguments{
\item{.Object}{a sleigh class object}
}

\examples{
\dontrun{
# simple hello world
s = sleigh()
rankCount(s)
}
}

\keyword{methods}



