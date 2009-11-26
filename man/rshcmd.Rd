\name{rshcmd}
\alias{rshcmd}
\title{Sleigh Auxiliary Function}
\description{
This function is used by the sleigh constructor when starting workers on
remote nodes using the \code{rsh} command.  Note that it doesn't actually start
any workers directly: it simply returns the program name and arguments
to start a worker on the specified node.
}

\usage{
rshcmd(host, options)
}

\arguments{
\item{host}{Name of the worker machine to be started.}
\item{options}{An environment or list that contains information used to
construct the command.  See \code{defaultSleighOptions} for more information.}
}

\value{
A character mode vector, whose first element is the command that will be
executed to start the worker, and whose subsequent elements are the
arguments to the command.
}

\note{
\code{rshcmd} should only be used if ssh is not available, since ssh is a much
more secure, modern replacement for rsh.
}

\details{
\code{rshcmd}currently uses the following options: \code{user}, \code{wrapperDir},
and \code{python}.  The \code{user} option is the most useful.
}



\seealso{\code{\link{sshcmd}}, \code{\link{sleigh}}}

\examples{
\dontrun{
# Create a sleigh with workers on nodes n1 and n2 started via rsh:
s <- sleigh(launch=rshcmd, nodeList=c('n1', 'n2'))

# Same as previous, but as user 'frank':
s <- sleigh(launch=rshcmd, nodeList=c('n1', 'n2'), user='frank')

# Specify an alternate python interpreter and see the command
# and arguments that rshcmd returns:
defaultSleighOptions$python <- '/usr/local/bin/python'
rshcmd('node1', defaultSleighOptions)
}
}

\keyword{utilities}
