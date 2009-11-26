\name{eachWorker}
\title{Execute a Function in Parallel on all Workers of a Sleigh}
\docType{methods}
\alias{eachWorker-methods}
\alias{eachWorker,sleigh-method}
\alias{eachWorker}
\usage{
  \S4method{eachWorker}{sleigh}(.Object, fun, \dots, eo=NULL, DEBUG=FALSE)
}
\arguments{
  \item{.Object}{sleigh class object.}
  \item{fun}{the function to be evaluated by each of the sleigh workers.
    In the case of functions like \code{+}, \code{\%*\%}, etc.,
    the function name must be quoted.}
  \item{\dots}{optional arguments to \code{fun}.}
  \item{eo}{additional options, see details}
  \item{DEBUG}{logical; should \code{browser} function be called upon entry to
    \code{eachWorker}?}
}
\description{
  \code{eachWorker} executes function \code{fun} once on each worker in
  the specified sleigh, and returns the results in a list.  This can be
  useful for initializing each of the workers before starting to execute
  tasks using \code{eachElem}.  Loading packages using the
  \code{library} function, loading data sets using the \code{data}
  function, and assigning variables in the global environment are common
  tasks for \code{eachWorker}.
}
\details{
  The \code{eo} argument is a list that can be used to specify various options.
  The current options are: \code{eo$blocking}, and \code{eo$accumulator}.

  The \code{eo$blocking} option is used to indicate whether to wait for the
  results, or to return as soon as the tasks have been submitted.  If
  set to \code{FALSE}, \code{eachWorker} will return a \code{sleighPending}
  object that is used to monitor the status of the tasks, and to
  eventually retrieve the results.  You must wait for the results to be
  complete before executing any further tasks on the sleigh, or an
  exception will be raised.  The default value is \code{TRUE}.

  The \code{eo$accumulator} option can be used to specify a function
  that will receive the results of the task execution.  Note that while
  this can be a very useful feature with \code{eachElem}, it's not
  commonly used with \code{eachWorker}, but is included for consistency.
  The first argument to \code{eo$accumulator} function is a list of
  results, where the length of the list is always equal to 1 (because
  there is no \code{eo$chunkSize} option in \code{eachWorker}).  The second
  argument is a vector of task numbers, starting from 1, where the
  length of the vector is also always equal to 1.  The task numbers are
  not very important when used with \code{eachWorker}, because the order
  of tasks isn't specified, as it is with \code{eachElem}.  Note that
  when \code{eo$accumulator} is specified, \code{eachWorker} returns
  \code{NULL}, not the list of results, since \code{eachWorker} doesn't save
  any of the results after passing them to the \code{eo$accumulator}
  function.

  The \code{DEBUG} argument is used call the \code{browser} function
  upon entering \code{eachWorker}.  The default value is \code{FALSE}.
}
\note{
  The \code{eo$blocking} option can be very useful for starting a
  function on each of the workers, and then allowing the master process
  to interact with the workers via NetWorkSpace operations in order to
  implement sophisticated parallel applications.
}
\seealso{
  \code{\link{eachElem}}, \code{\link{sleighPending}}
}
\examples{
  \dontrun{
# create a sleigh
s <- sleigh()

# assign to global variable x on each worker
eachWorker(s, function() x <<- 1)

# get a listing of each worker's global environment
eachWorker(s, function() ls(globalenv()))

# get system info from each worker
eachWorker(s, Sys.info)

# load MASS package on each worker
eachWorker(s, function() library(MASS))

# non-blocking example using simple NWS operations
sp <- eachWorker(s, function() nwsFind(SleighNws, 'hello'), 
        eo=list(blocking=FALSE))
nwsStore(s@nws, 'hello', 'world')
waitSleigh(sp)	
  }
}
\keyword{methods}
