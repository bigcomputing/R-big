\name{lsfSleigh}
\alias{lsfSleigh}
\title{sleigh Auxiliary Function}
\description{
  This function will properly check to see if the R script
  has been launched in a parallel fashion. If so, it will
  start jobs on the provided nodes using SSH. If not, it expects
  that the parameter \code{N} contains the number of nodes to start
  via \code{bsub}. Extra arguments to \code{bsub} can be provided using the
  \code{lsfoptions} parameter.
}

\keyword{methods}
\keyword{internal}
