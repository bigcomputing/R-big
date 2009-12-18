\name{launch}
\alias{launch}
\title{Sleigh Auxiliary Function}
\description{
  Setup appropropriate worker accounting information, and
  start the worker loop. 
}

\details{
  The only time clients ever need to invoke this function directly is when 
web launch is used. 
  The \code{runMe} variable contains a value that is the R function call to 
launch the worker. 
  For other launch types such as SSH and local launch, the \code{launch} 
function is invoked within \code{cmdLaunch}.
}
\seealso{\code{\link{cmdLaunch}}}

\keyword{methods}
\keyword{internal}
