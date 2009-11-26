\name{eachElem}
\title{Apply a Function in Parallel over a Set of Lists and Vectors}
\docType{methods}
\alias{eachElem-methods}
\alias{eachElem,sleigh-method}
\alias{eachElem}
\usage{
  \S4method{eachElem}{sleigh}(.Object, fun, elementArgs=list(), fixedArgs=list(), 
      eo=NULL, DEBUG=FALSE)
}
\arguments{
  \item{.Object}{sleigh class object.}
  \item{fun}{the function to be evaluated by the sleigh.
    In the case of functions like \code{+}, \code{\%*\%}, etc.,
    the function name must be quoted.}
  \item{elementArgs}{list of vectors, lists, matrices, and data frames that
    specify (some of) the arguments to be passed to \code{fun}. Each element should correspond to an argument of \code{fun}.}
  \item{fixedArgs}{list of additional arguments to be passed to \code{fun}. Each element should correspond to an argument of \code{fun}.}
  \item{eo}{list specifying environment options. See the section Environment Options below.}
  \item{DEBUG}{logical; should \code{browser} function be called upon entry to
    \code{eachElem}? The default is \code{FALSE}.}
}
\description{
  \code{eachElem} executes function \code{fun} multiple times in
  parallel with a varying set of arguments, and returns the results in a
  list.  It is functionally similar to the standard R
  \code{lapply} function, but is more flexible in the way that
  the function arguments can be specified.
}
\details{
  The \code{eachElem} function forms argument sets from objects passed in via
  \code{elementArgs} and \code{fixedArgs}.  
   The elements of \code{elementsArgs} are used to specify the arguments that are
  changing, or varying, from task to task, while the elements of
  \code{fixedArgs} are used to specify the arguments that do not vary
  from task to task.  The number of tasks that are executed by a call to
  \code{eachElem} is basically equal to the length of the longest vector
  (or list, etc) in \code{elementArgs}.  If any elements of
  \code{elementArgs} are shorter, then their values are recycled, using
  the standard R rules.

  The elements of \code{elementArgs} may be vectors, lists, matrices, or
  data frames.  The vectors and lists are always iterated over by
  element, or \code{"cell"}, but matrices and data frames can also be iterated
  over by row or column.  This is controlled by the \code{by} option,
  specified via the \code{eo} argument.  See below for more information.

  For example:\cr

    \code{eachElem(s, '+', elementArgs=list(1:4), fixedArgs=list(100))}\cr

  This will submit four tasks, since the length of 1:4 is four.  The
  four tasks will be to add the arguments 1 and 100, 2 and 100, 3 and
  100, and 4 and 100.  The result is a list containing the four values
  101, 102, 103, and 104.

  Another way to do the same thing is with:\cr

    \code{eachElem(s, '+', elementArgs=list(1:4, 100))}\cr

  Since the second element of \code{elementArgs} is length one, it's
  value is recycled four times, thus specifying the same set of tasks as
  in the previous example.  This method also has the advantage of making it
  easy to put fixed values before varying values, without the need for
  the \code{eo$argPermute} option, discussed later.  For example:\cr

    \code{eachElem(s, '-', elementArgs=list(100, 1:4))}

  is similar to the R statement:

    \code{100 - 1:4}

  Note that in simple examples like these, where the results are numeric
  values, the standard R \code{unlist} function can be very
  useful for converting the resulting list into a vector.
}
\section{Environment Options}{
  The \code{eo} argument is a list that can be used to specify various
  options.  The following options are recognized: 
  \describe{ 
  \item{elementFunc}{The \code{eo$elementFunc} option can be used to specify a callback function that provides the varying arguments for \code{fun} in place
  of \code{elementArgs} (that is, you can't specify both
  \code{eo$elementFunc} and \code{elementArgs}).  \code{eachElem} calls
  the \code{eo$elementFunc} function to get a list of arguments for one
  invocation of \code{fun}, and will keep calling it until
  \code{eo$elementFunc} signals that there are no more tasks to execute
  by calling the \code{stop} function with no arguments.
  \code{eachElem} appends any values specified by \code{fixedArgs} to
  the list returned by \code{eo$elementFunc} just as if
  \code{elementArgs} had been specified.
  
  \code{eachElem} passes the number of the desired task (starting from
  1) as the first argument to \code{eo$elementFunc}, and the value of
  the \code{eo$by} option as the second argument.  Note that the use of
  the \code{eo$elementFunc} function is an advanced feature, but is very
  useful when executing a large number of tasks, or when the arguments
  are coming from a database query, for example.  For that reason, the
  \code{eo$loadFactor} option should usually be used in conjunction with
  \code{eo$elementFunc} (see description below).}

  \item{accumulator}{The \code{eo$accumulator} option can be used to specify a callback
  function that will receive the results of the task execution as soon
  as they are complete, rather than returning all of the task results as
  a list when \code{eachElem} completes.  In other words,
  \code{eachElem} will call the \code{eo$accumulator} function with task
  results as soon as it receives them from the sleigh workers, rather
  than saving them in memory until all the tasks are complete.  Note
  that if the tasks are \emph{chunked} (using the \code{eo$chunkSize} option
  described below), then the \code{eo$accumulator} function will receive
  multiple task results, which is why the task results are always passed
  to the \code{eo$accumulator} function in a list.

  The first argument to the \code{eo$accumulator} function is a list of
  results, where the length of the list is equal to \code{eo$chunkSize}.
  The second argument is a vector of task numbers, starting from 1,
  where the length of the vector is also equal to \code{eo$chunkSize}.
  The task numbers are very important, because the results are not
  guaranteed to be returned in order.  \code{eo$accumulator} is another
  advanced feature, and like \code{eo$elementFunc}, is very useful when
  executing a large number of tasks.  It allows you to process each
  result as they finish, rather than forcing you to wait until all of
  the tasks are complete.  In conjunction with \code{eo$elementFunc} and
  \code{eo$loadFactor}, you can set up a pipeline, allowing you to
  process an unlimited number of tasks efficiently.  Note that when
  \code{eo$accumulator} is specified, \code{eachElem} returns NULL, not
  the list of results, since \code{eachElem} doesn't save any of the
  results after passing them to the \code{eo$accumulator} function.}

  \item{by}{The \code{eo$by} option specifies the iteration scheme to use for matrix
  and data frame elements in \code{elementArgs}.  The default value is
  \code{"row"}, but it can also be set to \code{"column"} or \code{"cell"}.  Vectors and
  lists in \code{elementArgs} are not affected by this option.}

  \item{chunkSize}{The \code{eo$chunkSize} option is a tuning parameter that
  specifies the number of tasks that sleigh workers should allocate at a
  time.  The default value is 1, but if the tasks are small, performance
  can be improved by specifying a larger value, which decreases the
  overhead per task.

  If the \code{fun} function executes very quickly, you may not be able
  to keep your workers busy, giving you poor performance.  In that case,
  consider setting the \code{eo$chunkSize} option to a large enough
  number to increase the effective task execution time.}

  \item{loadFactor}{The \code{eo$loadFactor} option is a tuning parameter that specifies
  the maximum number of tasks per worker that are submitted to the
  sleigh at the same time.  If set, no more than \code{(loadFactor *
  workerCount)} tasks will be submitted at the same time.  This helps to
  control the resource demands that are made on the NetWorkSpaces
  server, which is especially important if there are a large number of
  tasks.  Note that this option is ignored if \code{blocking} is set to
  \code{FALSE}, since the two options are incompatible with each other.

  If in doubt, set the \code{eo$loadFactor} option to 10.  That will
  almost certainly avoid putting a strain on the NetWorkSpaces server, and
  if that isn't enough to keep your workers busy, then you should 
  really be using the \code{eo$chunkSize} option to give the workers
  more to do.}

  \item{blocking}{The \code{eo$blocking} option is used to indicate whether to wait for the
  results, or to return as soon as the tasks have been submitted.  If
  set to \code{FALSE}, \code{eachElem} will return a \code{sleighPending}
  object that is used to monitor the status of the tasks, and to
  eventually retrieve the results.  You must wait for the results to be
  complete before executing any further tasks on the sleigh, or an
  exception will be raised.  The default value is \code{TRUE}.}

  \item{argPermute}{The \code{eo$argPermute} option is used to reorder the arguments passed
  to \code{fun}.  It is generally only useful if the \code{fixedArgs}
  argument has been specified, and some of those arguments need to
  precede the arguments specified via \code{elementArgs}.  Note that by
  using recycling of elements in \code{elementArgs}, the use of
  \code{fixedArgs} and \code{argPermute} can often be avoided entirely.}
}}
\note{
  If \code{elementArgs} or \code{fixedArgs} isn't a list,
  \code{eachElem} will automatically wrap it in a list.  This is a
  convenience that only works for passing in a single vector and matrix,
  however.

  If \code{elementArgs} or \code{fixedArgs} are named lists, then the
  names are used to map the values to the appropriate argument of
  \code{fun}.  This can be used as another technique to avoid the use of
  \code{eo$argPermute}.

  The \code{elementArgs} argument can be specified as a data frame.
  This works just like a named list, and therefore, the column names of
  the data frame must all correspond to arguments of \code{fun}.  Note
  that if the data frame has many rows, the performance may not be good
  due to the overhead of subsetting data frames in R.

  If you have a huge number of tasks, consider using the
  \code{eo$elementFunc}, \code{eo$accumulator}, and \code{eo$loadFactor}
  options.

  If \code{eo$elementFunc} returns a value that isn't a list,
  \code{eachElem} will automatically wrap that value in a list.

  The \code{eo$elementFunc} function doesn't have to define a second
  formal argument (the \code{by} argument) if it's not needed.

  The \code{eo$accumulator} function doesn't have to define a second
  formal argument (the \code{taskVector} argument) if it's not needed.
  Just remember that the results are not guaranteed to come back in
  order.
}
\seealso{
  \code{\link{eachWorker}}, \code{\link{sleighPending}}
}
\examples{
  \dontrun{
# create a sleigh
s <- sleigh()

# compute the list mean for each list element
x <- list(a=1:10, beta=exp(-3:3), logic=c(TRUE,FALSE,FALSE,TRUE))
eachElem(s, mean, list(x))

# median and quartiles for each list element
eachElem(s, quantile, elementArgs=list(x), fixedArgs=list(probs=1:3/4))

# use eo$elementFunc to supply 100 random values and eo$accumulator to
# receive the results
elementFunc <- function(i, by) {
  if (i <= 100) list(i=i, x=runif(1)) else stop()
}
accumulator <- function(resultList, taskVector) {
  if (resultList[[1]][[1]] != taskVector[1]) stop('assertion failure')
  cat(paste(resultList[[1]], collapse=' '), '\n')
}
eo <- list(elementFunc=elementFunc, accumulator=accumulator)
eachElem(s, function(i, x) list(i=i, x=x, xsq=x*x), eo=eo)
  }
}
\keyword{methods}
