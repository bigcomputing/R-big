\name{nwsOpenWs}
\docType{methods}
\alias{nwsOpenWs}
\alias{nwsOpenWs-methods}
\alias{nwsOpenWs,nwsServer-method}
\title{Create and Own a netWorkSpace}
\description{
Create and own a netWorkSpace, if it does not already exist. If the 
netWorkSpace already exists, but no one owns it, then caller claims 
ownership of the netWorkspace.  If the netWorkSpace already exists 
and someone already claimed the ownership of it, then caller
simply makes connection to the netWorkspace. 
}

\usage{
\S4method{nwsOpenWs}{nwsServer}(.Object, wsName, space=NULL, ...)
}

\arguments{
\item{.Object}{a nwsServer class object}
\item{wsName}{name of the netWorkSpace to be created}
\item{space}{a netWorkSpace class object}
\item{...}{optional arguments related to the persistent state of a netWorkSpace}
}

\details{
If the \code{space} argument is not provided, \code{nwsOpenWs} creates a shared 
netWorkSpace with the name \code{wsName} on the server represented by 
\code{.Object}. Otherwise, use the existing netWorkSpace object
provided by the \code{space} argument. 

The optional argument \code{persistent} is a logical value indicating 
whether the shared netWorkSpace is persistent or not. If the netWorkSpace 
is persisent (\code{persistent=TRUE}), then
the netWorkSpace is not deleted when the owner of the shared netWorkSpace 
exits. Otherwise, the netWorkSpace is deleted when its owner exits.
}


\seealso{\code{\link{nwsUseWs}}}

\examples{
\dontrun{
# example 1
nwss <- nwsServer()
ws <- nwsOpenWs(nwss, "nws example")

# example 2
xs <- nwsOpenWs(nwss, wsName='nws example', space=ws)

# example 3
ys <- nwsOpenWs(nwss, "persistent space", persistent=TRUE)
}
}

\keyword{methods}
