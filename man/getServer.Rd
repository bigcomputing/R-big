\name{getServer}
\alias{getServer}
\title{Get Server object}
\description{
  This function returns the server object registered using
  the \code{setServer} function.
  If a server object hasn't been registered, a default
  server, will be created and returned.
}

\usage{
getServer(\dots, create=FALSE)
}

\arguments{
\item{\dots}{currently ignored.}
\item{create}{used to force a new server to be created and registered,
 even if there already is one.}
}

\note{
  This function is primarily used within the nws package in to order to
  ease object creation and specify an ad-hoc server setup with ease.
  If the function is called before a call to \code{setServer} has been made, it
  will create, store, and return a \code{managedServerInfo} object created
  with the default options.
}

\seealso{\code{\link{setServer}}}

\examples{
\dontrun{
hostname <- nwsHost(getServer())
}
}

\keyword{utilities}
