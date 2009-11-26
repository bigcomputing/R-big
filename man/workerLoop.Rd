\name{workerLoop}
\alias{workerLoop}
\title{sleigh Auxiliary Function}
\description{
  Worker waits and listens for tasks in the sleigh workspace. 
  Worker then works on the retrieved
  tasks and return results back to the sleigh workspace. 
  This loop ends when \code{stopSleigh} is invoked.
}

\details{
  This function is invoked by \code{launch} function. It is not meant
  to be called directly by clients. 
}
\seealso{\code{\link{launch}}}

\keyword{methods}
\keyword{internal}
