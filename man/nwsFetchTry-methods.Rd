\name{nwsFetchTry}
\docType{methods}
\alias{nwsFetchTry}
\alias{nwsFetchTry-methods}
\alias{nwsFetchTry,netWorkSpace-method}
\title{Fetch a Value from a NetWorkSpace (Non-Blocking Version)}
\description{
Attempt to fetch a value associated with a variable from the shared netWorkSpace; 
a non-blocking version of \code{\link{nwsFetch}}.
}

\usage{
\S4method{nwsFetchTry}{netWorkSpace}(.Object, xName, defaultVal=NULL)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of the variable to be fetched}
\item{defaultVal}{value to return, if xName is not found in the netWorkSpace}
}

\details{
Look in the shared netWorkSpace for a value bound to \code{xName}.  If found,
remove a value associated with \code{xName} from the shared netWorkSpace.
This operation is atomic. If there are multiple 
nwsFetch or nwsFetchTry requests for a given \code{xName}, any given value from 
the set of values associated with \code{xName} will be returned to just one requester. 

If variable is not found, return immediately rather than block on the 
operation (as in the case of \code{\link{nwsFetch}}). If variable is not found, 
the value of argument \code{defaultVal} is returned. By default, \code{defaultVal} is \code{NULL}.
}
\seealso{ \code{\link{nwsDeclare}}, \code{\link{nwsFetch}} }

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
# If variable 'x' is not found in the shared netWorkSpace,
# return default value, NULL.
nwsFetchTry(ws, 'x')
# If variable 'x' is not found in the shared netWorkSpace, 
# return 10.
nwsFetchTry(ws, 'x', 10)
}
}

\keyword{methods}
