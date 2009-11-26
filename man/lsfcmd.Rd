\name{lsfcmd}
\alias{lsfcmd}
\title{Sleigh Auxiliary Function}
\description{
This function is used by the sleigh constructor when starting workers on
remote nodes using the LSF \code{bsub} command.  Note that it doesn't 
actually start any workers directly: it simply returns the program 
name and arguments to start a worker on the specified node.
}

\usage{
lsfcmd(host, options)
}

\arguments{
\item{host}{Name from the \code{nodeList}.  This is currently ignored.}
\item{options}{An environment or list.  This is currently ignored.}
}

\value{
The character vector \code{c('bsub')}.
}

\details{
\code{lsfcmd} is not intended to be called by the user.
It is called by the sleigh constructor when specified via the
sleigh \code{launch} argument.  You may want to execute it when debugging
your sleigh option settings, but that can also be accomplished by
setting the sleigh \code{verbose} argument to \code{TRUE}.
}



\seealso{\code{\link{sleigh}}}

\examples{
\dontrun{
# Create a sleigh with workers on nodes n1 and n2 started via lsf:
s <- sleigh(launch=lsfcmd, nodeList=rep('fake', 10))
}
}

\keyword{utilities}
