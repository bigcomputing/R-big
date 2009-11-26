\name{scriptcmd}
\alias{scriptcmd}
\title{Sleigh Auxiliary Function}
\description{
This function is set through optional argument \code{scriptExec} in the sleigh
constructor. It uses Python to execute a worker script. 
}

\details{
This function indicates that configuration variables 
such as \code{RSleighName} and \code{RSleighNwsName} are passed as part of 
the worker script arguments rather than through the \code{env} command. 
This is often useful on systems that do not support the \code{env} command.

\code{scriptcmd} is the default choice to execute worker scripts on Windows.
}

\examples{
\dontrun{
s = sleigh(scriptExec=scriptcmd)
}
}

\keyword{methods}
\keyword{internal}
