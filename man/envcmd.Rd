\name{envcmd}
\alias{envcmd}
\title{Sleigh Auxiliary Function}
\description{
This function is set through optional argument \code{scriptExec} in the sleigh
constructor. 
}

\details{
This function indicates that configuration variables 
such as \code{RSleighName} and \code{RSleighNwsName} are exported to 
the environment using the \code{env} command. 

\code{envcmd} is the default choice to execute worker scripts on UNIX.
}

\examples{
\dontrun{
s = sleigh(scriptExec=envcmd)
}
}

\keyword{methods}
\keyword{internal}
