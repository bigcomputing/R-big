\name{managedServerInfo}
\docType{class}
\alias{managedServerInfo-class}
\alias{managedServerInfo}
\alias{initialize,managedServerInfo-method}
\title{Class Representing Managed NetWorkSpaces Server}
\description{Class representing managedServerInfo.}
\section{Objects from the Class}{
Objects can be created by calls of the form \cr
\code{managedServerInfo(...)}\cr
or\cr
\code{new("managedServerInfo", ...)}\cr
where '...' can be one or more of the following named arguements:
\describe{
   \item{\code{host}:}{server host name. Defaults to any interface.}
   \item{\code{port}:}{server port number. Defaults to any port.}
   \item{\code{webPort}:}{server web port number. Defaults to any port.}
   \item{\code{quiet}:}{quiet flag. Default
     value is \code{FALSE}.}
   \item{\code{pluginPath}:}{path to server plugins. If \code{NULL},
     will point to plugins in the \code{nwsserver} package.}
   \item{\code{logFile}:}{path of server log file. Defaults to no log file.}
}
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "managedServerInfo")}:
      managedServerInfo class constructor.}
    \item{nwsHost}{\code{signature(.Object = "managedServerInfo")}:
      return hostname associated with server. If bound to any interface,
    will return \code{'0.0.0.0'}.}
    \item{nwsPort}{\code{signature(.Object = "managedServerInfo")}:
      return port associated with server.}
    \item{nwsIsRunning}{\code{signature(.Object = "managedServerInfo")}:
      returns \code{TRUE} if the server is running, else \code{FALSE}.}
    \item{nwsWebPort}{\code{signature(.Object = "managedServerInfo")}:
      return the web port for the server.}
    \item{nwsServerInfoStop}{\code{signature(.Object = "managedServerInfo")}:
      shut down the server.}
  }
}

\examples{
\dontrun{
# example 1
sI = managedServerInfo()
# Or,
sI = new("managedServerInfo")

# example 2
sI = managedServerInfo(host="node1", port=5555, webPort=5556,
quiet=TRUE')
}
}
\keyword{classes}
