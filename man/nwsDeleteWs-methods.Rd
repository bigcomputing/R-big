\name{nwsDeleteWs}
\docType{methods}
\alias{nwsDeleteWs}
\alias{nwsDeleteWs-methods}
\alias{nwsDeleteWs,nwsServer-method}
\title{Delete a netWorkSpace}
\description{
Delete a shared netWorkSpace from the netWorkSpaces server.
}

\usage{
\S4method{nwsDeleteWs}{nwsServer}(.Object, wsName)
}

\arguments{
\item{.Object}{a nwsServer class object}
\item{wsName}{name of the netWorkSpace to be deleted}
}

\examples{
\dontrun{
# example 1
nwss <- nwsServer()
ws <- nwsOpenWs(nwss, "nws example")
# do some works
# ...
nwsDeleteWs(nwss, "nws example")

# example 2 illustrates accessing a server object 
# from the netWorkSpace class object
ws <- netWorkSpace("nws example 2")
# do some works 
# ...
nwsDeleteWs(ws@server, "nws example 2")
}
}

\keyword{methods}

