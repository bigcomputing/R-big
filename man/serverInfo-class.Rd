\name{serverInfo}
\docType{class}
\alias{serverInfo-class}
\alias{serverInfo}
\alias{initialize,serverInfo-method}
\title{Class Representing NetWorkSpaces Server}
\description{Class representing serverInfo.}
\section{Objects from the Class}{
Objects can be created by calls of the form \cr
\code{serverInfo(...)}\cr
or\cr
\code{new("serverInfo", ...)}\cr
where '...' can be one or more of the following named arguments:
\describe{
   \item{\code{host}:}{server host name. Default value is \code{"localhost"}.}
   \item{\code{port}:}{server port number. Default value is \code{8765}.}
}
}
\section{Slots}{
  \describe{
    \item{\code{host}:}{Object of class \code{"character"} representation of the server host name.}
    \item{\code{port}:}{Object of class \code{"numeric"} representation of the server port number.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "serverInfo")}:
      serverInfo class constructor.}
    \item{nwsHost}{\code{signature(.Object = "serverInfo")}:
      return hostname associated with server.}
    \item{nwsPort}{\code{signature(.Object = "serverInfo")}:
      return port associated with server.}
  }
}

\examples{
\dontrun{
# example 1
nwss = serverInfo()
# Or,
nwss = new("serverInfo")

# example 2
nwss = serverInfo(host="node1", port=5555)
}
}
\keyword{classes}
