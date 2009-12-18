\name{sshcmd}
\alias{sshcmd}
\alias{sshforwardcmd}
\title{Sleigh Auxiliary Function}
\description{
This function is used by the sleigh constructor when starting workers on
remote nodes using the \code{ssh} command.  Note that it doesn't actually start
any workers directly: it simply returns the program name and arguments
to start a worker on the specified node.
}

\usage{
sshcmd(host, options)
sshforwardcmd(host, options)
}

\note{
\code{sshcmd} and \code{sshforwardcmd} are not intended to be 
called directly by the user.
They are called by the sleigh constructor when specified via the
\code{launch} argument.  You may want to execute it when debugging
your sleigh option settings, but that can also be accomplished by
setting the \code{verbose} argument to \code{TRUE}.
}

\details{
\code{sshcmd} currently uses the following options: \code{user}, 
\code{wrapperDir}, \code{workerWrapper}, and \code{python}.  The 
\code{user} option is the most useful.

\code{sshforward} works like \code{sshcmd}, but it makes use of the 
\code{ssh -R} argument to tunnel the NWS server connection back to the master
machine.  It uses the options \code{nwsHostRemote} and \code{nwsPortRemote}
to determine what bind address and port should be forwarded back to
the master machine.

Note that when using \code{sshforwardcmd}, you must specify a different value
of \code{nwsHostRemote}, usually \code{'localhost'}.
}

\arguments{
\item{host}{Name of the worker machine to be started.}
\item{options}{An environment or list that contains information used to
construct the command.  See \code{defaultSleighOptions} for more information.}
}

\value{
A character vector, whose first element is the command that will be
executed to start the worker, and whose subsequent elements are the
arguments to the command.
}

\seealso{\code{\link{sleigh}}, \code{\link{defaultSleighOptions}}}

\examples{
\dontrun{
# Create a sleigh with workers on nodes n1 and n2 started via ssh:
s <- sleigh(launch=sshcmd, nodeList=c('n1', 'n2'))

# Same as previous, but as user 'frank':
s <- sleigh(launch=sshcmd, nodeList=c('n1', 'n2'), user='frank')

# Create two tunnels from workers n1 and n2 back the nws server
# on the local machine:
s <- sleigh(launch=sshforwardcmd, nodeList=c('n1', 'n2'),
            nwsHostRemote='localhost')

# Same as the previous example, but use port 9876 in case either
# worker machine already has an nws server bound to port 8765:
s <- sleigh(launch=sshforwardcmd, nodeList=c('n1', 'n2'),
            nwsHostRemote='localhost', nwsPortRemote=9876)
}
}

\keyword{utilities}
