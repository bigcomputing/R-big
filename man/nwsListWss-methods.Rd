\name{nwsListWss}
\docType{methods}
\alias{nwsListWss}
\alias{nwsListWss-methods}
\alias{nwsListWss,nwsServer-method}
\title{List All netWorkSpaces}
\description{
List all netWorkSpaces in the netWorkSpaces server.
}

\usage{
\S4method{nwsListWss}{nwsServer}(.Object, showDataFrame=TRUE)
}

\arguments{
  \item{.Object}{a nwsServer class object}
  \item{showDataFrame}{show result in data frame or string}
}

\details{
If \code{showDataFrame} is set to \code{TRUE} (the default), then the return value 
is a data frame with these columns: \code{Owned}, \code{Name}, 
\code{Owner}, \code{Persistent}, \code{NumVariables}, and \code{Variables}.

If \code{showDataFrame} is set to \code{FALSE}, then
the return value is a text string containing a list of workspaces in the
netWorkSpaces server. To see list output clearly, use: \cr
\code{write(nwsListWss(.Object, showDataFrame=FALSE), stdout())}
}

\examples{
\dontrun{
# example 1
nwss <- nwsServer()
ws1 <- nwsOpenWs(nwss, 'my space')
ws2 <- nwsOpenWs(nwss, 'other space')
write(nwsListWss(nwss, showDataFrame=FALSE), stdout())

# example 2
# retrieve all workspace names
df <- nwsListWss(nwss)
df$Name
$"1"
[1] "__default"

$"2"
[1] "my space"

$"3"
[1] "other space"
}
}

\keyword{methods}
