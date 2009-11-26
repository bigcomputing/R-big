\name{nwsFindTry}
\docType{methods}
\alias{nwsFindTry}
\alias{nwsFindTry-methods}
\alias{nwsFindTry,netWorkSpace-method}
\title{Find a Stored Value (Non-Blocking Version)}

\description{
Attempt to find a value associated with a variable from the shared netWorkSpace; 
a non-blocking version of \code{\link{nwsFind}}.
}

\usage{
\S4method{nwsFindTry}{netWorkSpace}(.Object, xName, defaultVal=NULL)
}

\arguments{
\item{.Object}{a netWorkSpace class object}
\item{xName}{name of variable to be found}
\item{defaultVal}{value to return if xName is not found}
}

\details{
The \code{nwsFindTry} method looks in the shared netWorkSpace \code{.Object}
for a value bound to \code{xName}; if it finds such a value, \code{nwsFindTry}
returns it but does not remove it. 
If it does not find a value, \code{nwsFindTry} returns immediately, rather than 
blocking as in the case of \code{\link{nwsFind}}), and
the value of argument \code{defaultVal} is returned. 
By default, \code{defaultVal} is \code{NULL}.

If there is more than one value associated with \code{xName}, the
particular value returned depends on \code{xName}'s behavior. See
\code{\link{nwsDeclare}} for details.
}

\seealso{\code{\link{nwsDeclare}}, \code{\link{nwsFind}}}


\examples{
\dontrun{
ws <- netWorkSpace('nws example')
x <- nwsFindTry(ws, 'abc', -1)
}
}

\keyword{methods}
