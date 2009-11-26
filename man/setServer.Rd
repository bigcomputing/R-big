\name{setServer}
\alias{setServer}
\title{Set Server object}
\description{
  This function registers the specified server object.
  It can be accessed later using the \code{getServer} function.
}

\usage{
setServer(serverInfo, \dots)
}

\arguments{
\item{serverInfo}{serverInfo object to register.}
\item{\dots}{currently ignored.}
}

\note{
  This function can be used to specify a server the user wishes to be
  used by the nws package itself, or by any package which uses the
  \code{getServer} function.
}

\seealso{\code{\link{getServer}}}

\examples{
\dontrun{
sI <- managedServerInfo()
setServer(sI)
#create a sleigh on sI 
s <- sleigh()
}
}

\keyword{utilities}
