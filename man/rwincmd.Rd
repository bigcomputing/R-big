\name{rwincmd}
\alias{rwincmd}
\title{Sleigh Auxiliary Function}
\description{
This function indicates that WMI will be used to launch sleigh workers. 
}
\seealso{\code{\link{sleigh}}}

\details{
This function should only be used on Windows.
This function is only meant to be passed through optional argument, 'launch', 
in the sleigh constructor.
}

\examples{
\dontrun{
s <- sleigh(launch=rwincmd, nodeList=c('n1', 'n2'), passwd='bogus')
}
}

\keyword{methods}
\keyword{internal}
