\name{view}
\docType{methods}
\alias{view}
\alias{view-methods}
\alias{view,sleigh-method}
\alias{view,nwsServer-method}
\alias{view,netWorkSpace-method}
\alias{view,managedServerInfo-method}
\title{View object in the NWS Space Monitor}
\description{
This provides a convenient way to view the specified nwsServer, netWorkSpace,
or sleigh workspace in the NWS Space Monitor using your browser.
}

\usage{
\S4method{view}{nwsServer}(.Object, ...)
\S4method{view}{netWorkSpace}(.Object, ...)
\S4method{view}{sleigh}(.Object, ws=c('system', 'user'), ...)
\S4method{view}{managedServerInfo}(.Object, ...)
}

\arguments{
\item{.Object}{an object of class \code{sleigh}, \code{nwsServer}, \code{managedServerInfo},
or \code{netWorkSpace}.}
\item{ws}{the type of sleigh workspace to view.}
\item{...}{currently unused.}
}

\examples{
\dontrun{
# create a sleigh and view its user workspace
s = sleigh()
view(s, ws='user')

# create a nwsServer and list all workspaces in it
srv = nwsServer()
view(srv)
}
}

\keyword{methods}
