\name{sleighPending}
\docType{class}
\alias{sleighPending-class}
\alias{sleighPending}
\alias{initialize,sleighPending-method}
\title{Class ``sleighPending"}
\description{Class representing sleighPending.}
\section{Objects from the Class}{
Objects can be created by calls of the form \cr 
\code{sleighPending(nws, numTasks, bn, ss)} \cr
or \cr
\code{new("sleighPending", nws, numTasks, bn, ss)}.
\describe{
  \item{\code{nws}:}{netWorkSpace class object.}
  \item{\code{numTasks}:}{number of submitted tasks.}
  \item{\code{bn}:}{barrier names.}
  \item{\code{ss}:}{sleigh state.}
}
}

\section{Slots}{
  \describe{
    \item{\code{nws}:}{Object of class \code{"netWorkSpace"}.} 
    \item{\code{numTasks}:}{The number of pending tasks in sleigh.}
    \item{\code{barrierName}:}{Character string giving the barrier name.}
    \item{\code{sleighState}:}{Object of class \code{"environment"} representing the sleigh state.}
    \item{\code{state}:}{Object of class \code{"environment"} representing the \code{sleighPending} state.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "sleighPending")}: \code{sleighPending} class constructor.}
    \item{checkSleigh}{\code{signature(.Object = "sleighPending")}: returns 
the number of results yet to be generated for the pending sleigh job.}
    \item{waitSleigh}{\code{signature(.Object = "sleighPending")}: wait and block for the results to be generated for the pending sleigh job.}
  }
}

\details{
This class object is usually obtained from non-blocking \code{eachElem}
or non-blocking \code{eachWorker}. 
}

\seealso{ \code{\link{eachWorker}}, \code{\link{eachElem}} }


\keyword{classes}
\keyword{methods}
