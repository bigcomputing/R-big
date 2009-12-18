\name{nwsListVars}
\docType{methods}
\alias{nwsListVars}
\alias{nwsListVars-methods}
\alias{nwsListVars,netWorkSpace-method}
\title{List Variables in a netWorkSpace}
\description{
List variables in a netWorkSpace.
}

\usage{
\S4method{nwsListVars}{netWorkSpace}(.Object, wsName='', showDataFrame=TRUE)
}

\arguments{
  \item{.Object}{a netWorkSpace class object.}
  \item{wsName}{character string specifying the name of the netWorkSpace.}
  \item{showDataFrame}{logical flag specifying whether to show result in 
data frame or string.}
}

\details{
Return listing of variables in the netWorkSpace with the name passed by 
\code{wsName} argument.
If \code{wsName} is empty, then return variables in the netWorkSpace
represented by \code{.Object}.

The return values from \code{nwsListVars} can be represented in a 
string or a data frame.
If \code{showDataFrame} is set to \code{TRUE} (the default),
then the listing is returned in a
data frame with these fields: \code{Variables}, \code{NumValues}, \code{NumFetchers},
\code{NumFinders}, and \code{Mode}.

If \code{showDataFrame} is set to \code{FALSE}, then
the listing is returned in a string. To see list output clearly,
use:\cr 
\code{write(nwsListVars(.Object, showDataFrame=FALSE), stdout())}\cr
}

\examples{
\dontrun{
# example 1
ws <- netWorkSpace('nws example')
write(nwsListVars(ws, showDataFrame=FALSE), stdout())
}
}

\keyword{methods}
