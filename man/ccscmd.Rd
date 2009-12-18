\name{ccscmd}
\alias{ccscmd}
\title{Sleigh Auxiliary Function}
\description{
This function indicates that CCS will be used to launch sleigh workers. 
}
\seealso{\code{\link{sleigh}}}

\details{
This function is only meant to be passed through optional argument, 'launch', 
in the sleigh constructor.
}

\examples{
\dontrun{
s = sleigh(launch=ccscmd)
}
}

\keyword{methods}
\keyword{internal}
