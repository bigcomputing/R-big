\name{nwsDeclare}
\docType{methods}
\alias{nwsDeclare}
\alias{nwsDeclare-methods}
\alias{nwsDeclare,netWorkSpace-method}
\title{Declare a netWorkSpace Variable}
\description{
Declare a variable with particular mode in a shared netWorkSpace.
}

\usage{
  nwsDeclare(.Object, xName, mode)
}

\arguments{
  \item{.Object}{a netWorkSpace class object.}
  \item{xName}{character string giving the name of variable to be declared.}
  \item{mode}{character string specifying the mode of the variable, see Details.}
}

\details{
If \code{xName} has not already been declared in the netWorkSpace, the behavior
of \code{xName} will be determined by \code{mode}. Possible values are
\code{'fifo'} (the default), \code{'lifo'}, \code{'multi'}, or 
\code{'single'}. In the first three cases,
multiple values can be associated with \code{xName}. When a value is
retrieved for \code{xName}, the oldest value stored will be used in
\code{'fifo'} mode, the youngest in \code{'lifo'} mode, and a
nondeterministic choice will be made in \code{'multi'} mode. In
'single' mode, only the most recent value is retained.
}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')
nwsDeclare(ws, 'pi', 'single')
nwsStore(ws, 'pi', 2.171828182)
nwsStore(ws, 'pi', 3.141592654)
nwsListVars(ws)  # shows that only the most recent value of pi is retained
}
}

\keyword{methods}
