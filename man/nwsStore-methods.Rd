\name{nwsStore}
\docType{methods}
\alias{nwsStore}
\alias{nwsStore-methods}
\alias{nwsStore,netWorkSpace-method}
\title{Associate a Value with a Variable in netWorkSpace}
\description{
Store value associated with a variable in the shared netWorkSpace.
}

\usage{
\S4method{nwsStore}{netWorkSpace}(.Object, xName, xVal)
}

\arguments{
\item{.Object}{a netWorkSpace class object.}
\item{xName}{character string to be used as the name of the stored variable.}
\item{xVal}{value to be stored.}
}


\details{
The \code{nwsStore} method associates the value \code{xVal} with the 
variable \code{xName} in the
shared netWorkSpace \code{.Object}, thereby making the
value available to all the distributed R processes.  If a mode
has not already been set for \code{xName}, \code{'fifo'} will be used (see
\code{\link{nwsDeclare}}).

Note that, by default (\code{'fifo'} mode), \code{nwsStore} is not idempotent: 
repeating \code{nwsStore (nws, xName, xVal)} will add additional values to the 
set of values associated with the variable named \code{xName}. See the examples
below for details.
}

\seealso{\code{\link{nwsDeclare}}}

\examples{
\dontrun{
ws <- netWorkSpace('nws example')

# To store value 5 bound to variable 'x' on the netWorkSpace 'ws'
# (If 'x' was declared, then its mode is inherited, 
# otherwise 'x' uses the default mode 'fifo')
nwsStore(ws, 'x', 5)

# store 10 values associate with variable y to the netWorkSpace
for (i in 1:10)
  nwsStore(ws, 'y', i)

# retrieve 10 values associate with variable y from the netWorkSpace
for (i in 1:10)
  print(nwsFetch(ws, 'y'))
}
}

\keyword{methods}
