\name{workerInfo}
\docType{methods}
\alias{workerInfo}
\alias{workerInfo-methods}
\alias{workerInfo,sleigh-method}
\title{sleigh Class Method}
\description{
Return information about sleigh workers.
}

\usage{
\S4method{workerInfo}{sleigh}(.Object)
}

\arguments{
\item{.Object}{a sleigh class object.}
}

\value{
A data frame with a row for each worker, with column names 'host', 'os',
'pid', 'R', 'nws', 'rank', and 'logfile'.
}

\examples{
\dontrun{
s <- sleigh()
status(s, TRUE, 20)
workerInfo(s)
}
}

\keyword{methods}
