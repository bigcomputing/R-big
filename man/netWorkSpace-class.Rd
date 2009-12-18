\name{netWorkSpace}
\docType{class}
\alias{netWorkSpace-class}
\alias{netWorkSpace}
\alias{initialize,netWorkSpace-method}

\title{NetWorkSpaces Objects}
\description{NetWorkSpaces provides a framework to coordinate R programs. NetWorkSpaces objects represent a generalized workspace or environment.  Two or more R programs communicate data by storing it in and retrieving it from the NetWorkSpace object.}
\section{Objects from the Class}{
Objects can be created by calls of the form \cr 
\code{netWorkSpace(wsName, ...)} \cr
or \cr
\code{new("netWorkSpace", wsName, ...)}\cr
where '...' can be one or more of the following named arguments:
\describe{
  \item{\code{wsName}}{name of the netWorkSpace to be created.}
  \item{\code{serverInfo}}{object of type
    \code{serverInfo} or \code{managedServerInfo}. If not
    specified, will get the return value of \code{getServer}.}
  \item{\code{useUse}}{a logical value indicating whether ownership will be claimed for this netWorkSpace. By default,
	\code{useUse=FALSE}, which means ownership will be claimed.}
  \item{\code{serverWrap}}{a netWorkSpaces server object. Reuse an existing server connection, instead of creating a new server connection.}
}

}
\note{
  The constructor still supports the \code{serverHost} and \code{port}
  arguements, but as of release 2.0, they are deprecated and will be
  removed in 4.0.
  The legacy descriptions are:
  
  \code{serverHost}: host name of the server this netWorkSpace will be connected to. By default, local machine is used.

  \code{port}: port number of the server this netWorkSpace will be
    connected to. Default port number is 8765.

  If the user specifies both one or more deprecated options and a serverInfo
  object, the deprecated option(s) will be ignored.
}
\seealso{
  \code{\link{defaultNetWorkSpaceOptions}}
  }
\section{Slots}{
  \describe{
    \item{\code{server}}{Object of class \code{"nwsServer"} representation of the server that this netWorkSpace connects to.}
    \item{\code{wsName}}{Object of class \code{"character"} representation of this netWorkSpace's name.}
  }
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "netWorkSpace")}: netWorkSpace class constructor.}
    \item{\code{nwsFetch}}{\code{signature(.Object = "netWorkSpace")}: fetch a value of a workspace variable.}
    \item{\code{nwsFetchTry}}{\code{signature(.Object = "netWorkSpace")}: try to fetch a value of a workspace variable.}
    \item{\code{nwsFind}}{\code{signature(.Object = "netWorkSpace")}: find a value of a workspace variable.}
    \item{\code{nwsFindTry}}{\code{signature(.Object = "netWorkSpace")}: try to find a value of a workspace variable.}
    \item{\code{nwsStore}}{\code{signature(.Object = "netWorkSpace")}: store a value into a workspace variable.}
    \item{\code{nwsFetchFile}}{\code{signature(.Object = "netWorkSpace")}: fetch a value of a workspace variable and write it to a file.}
    \item{\code{nwsFetchTryFile}}{\code{signature(.Object = "netWorkSpace")}: try to fetch a value of a workspace variable and write it to a file.}
    \item{\code{nwsFindFile}}{\code{signature(.Object = "netWorkSpace")}: find a value of a workspace variable and write it to a file.}
    \item{\code{nwsFindTryFile}}{\code{signature(.Object = "netWorkSpace")}: try to find a value of a workspace variable and write it to a file.}
    \item{\code{nwsStoreFile}}{\code{signature(.Object = "netWorkSpace")}: store data from a file into a workspace variable.}
    \item{\code{nwsIFetch}}{\code{signature(.Object = "netWorkSpace")}:
      create a function that acts as a destructive iterator over the values of the specified variable.}
    \item{\code{nwsIFetchTry}}{\code{signature(.Object = "netWorkSpace")}:
      create a function that acts as a destructive iterator over the values of the specified variable.}
    \item{\code{nwsIFind}}{\code{signature(.Object = "netWorkSpace")}:
      create a function that acts as a non-destructive iterator over the values of the specified variable.}
    \item{\code{nwsIFindTry}}{\code{signature(.Object = "netWorkSpace")}:
      create a function that acts as a non-destructive iterator over the values of the specified variable.}
    \item{\code{nwsClose}}{\code{signature(.Object = "netWorkSpace")}: close the connection to the NWS server.}
    \item{\code{nwsDeclare}}{\code{signature(.Object = "netWorkSpace")}: declare the mode of a workspace variable.}
    \item{\code{nwsDeleteVar}}{\code{signature(.Object = "netWorkSpace")}: delete a variable from a workspace.}
    \item{\code{nwsListVars}}{\code{signature(.Object = "netWorkSpace")}: list all variables in a workspace.}
    \item{\code{ServerObject}}{\code{signature(.Object = "netWorkSpace")}: return the associated NwsServer object.}
    \item{\code{nwsVariable}}{\code{signature(.Object = "netWorkSpace")}: create an Active Binding for a NetWorkSpace Variable}
    \item{\code{nwsWsName}}{\code{signature(.Object = "netWorkSpace")}: return the name of the workspace.}
  }
}

\examples{
\dontrun{
# To create a new workspace with the name "my space" use:
ws = netWorkSpace('my space')

# To create a new workspace called "my space2" on nws server 
# running on port 8245 on machine zeus:
ws2 = netWorkSpace(wsName='my space2', serverInfo=serverInfo(host='zeus',
                   port=8245))
}
}

\keyword{classes}
\keyword{methods}
