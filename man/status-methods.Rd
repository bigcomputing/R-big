\name{status}
\docType{methods}
\alias{status}
\alias{status-methods}
\alias{status,sleigh-method}
\title{Check Status of Sleigh Workers}
\description{
  Check status of sleigh workers. 
}

\usage{
\S4method{status}{sleigh}(.Object, closeGroup=FALSE, timeout=0)
}

\arguments{
  \item{.Object}{a sleigh class object}
  \item{closeGroup}{a logical value indicating whether to close the
    worker group}
  \item{timeout}{timeout value measure in seconds}
}

\value{a list that contains two values: 
\item{numWorkers}{the number of workers started} 
\item{closed}{If \code{closed} is zero, then some workers failed to start.
  If \code{closed} is one, then either all workers have started or
  \code{closeGroup} is set to TRUE.} 
}
\details{
The argument \code{closeGroup} is set to \code{FALSE} by default. If 
\code{closeGroup} is set to \code{FALSE},
then all workers must wait for the others to start before they can start
working on tasks. If \code{closeGroup} is set to \code{TRUE}, no new 
workers may join the group after the timeout value has expired. Once the 
group is closed, all launched workers can start working on tasks. 
 
The \code{timeout} argument indicates how long to wait and check on the status
of workers.
}

\examples{
\dontrun{
# example 1
# one available machine and one non-existent machine
s <- sleigh(c('localhost', 'noname'))
slist <- status(s)
# slist$numWorkers = the number of worker started
# slist$closed = whether the worker group is closed or not. 

# example 2
# check the status of worker group after 20 seconds
slist <- status(s, timeout=20)

# example 3
# close the group after 10 seconds, regardless of whether
# all workers have started up successfully.
slist <- status(s, closeGroup=TRUE, timeout=10)
}
}


\keyword{methods}

