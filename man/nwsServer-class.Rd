\name{nwsServer}
\docType{class}
\alias{nwsServer-class}
\alias{nwsServer}
\alias{initialize,nwsServer-method}
\title{Class Representing The Connection to the NetWorkSpaces Server}
\description{Class representing nwsServer.}
\section{Objects from the Class}{
Objects can be created by calls of the form \cr
\code{nwsServer(...)}\cr
or\cr
\code{new("nwsServer", ...)}\cr
where '...' can be one or more of the following named arguments:
\describe{
  \item{\code{serverInfo}:}{object of type
    \code{serverInfo} or \code{managedServerInfo}. If not
    specified, will get the return value of \code{getServer}.}
}
}
\note{
  The constructor still supports the \code{serverHost} and \code{port}
  arguements, but as of release 2.0, they are deprecated and will be
  removed in 4.0.
  The legacy descriptions are:

  \code{serverHost}: server host name. Default value is \code{"localhost"}.

  \code{port}: server port number. Default value is \code{8765}.

  If the user specifies both one or more deprecated options and a serverInfo
  object, the deprecated option(s) will be ignored.
}
\seealso{
  \code{\link{defaultNwsServerOptions}}
  }
\section{Slots}{
  \describe{
    \item{\code{nwsSocket}:}{Object of class \code{"ANY"} representation of the socket connection to the server.}
    \item{\code{serverInfo}:}{Object of class \code{"ANY"} representation of the server.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "nwsServer")}: nwsServer class constructor.}
    \item{nwsDeleteWs}{\code{signature(.Object = "nwsServer")}: delete a netWorkSpace from the server.}
    \item{nwsListWss}{\code{signature(.Object = "nwsServer")}: list all netWorkSpaces in the server.}
    \item{nwsMktempWs}{\code{signature(.Object = "nwsServer")}: create a unique temporary workspace using the default or specified template.}
    \item{nwsOpenWs}{\code{signature(.Object = "nwsServer")}: create and owned a netWorkSpace.}
    \item{nwsUseWs}{\code{signature(.Object = "nwsServer")}: connect to a netWorkSpace but does not claim ownership.}
  }
}
\examples{
\dontrun{
# example 1
nwss = nwsServer()
# Or,
nwss = new("nwsServer")

# example 2
nwss = nwsServer(serverInfo=managedServerInfo())
}
}
\keyword{classes}
