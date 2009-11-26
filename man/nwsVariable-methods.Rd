\name{nwsVariable}
\title{Create an Active Binding for a netWorkSpace Variable}
\docType{methods}
\alias{nwsVariable-methods}
\alias{nwsVariable,netWorkSpace-method}
\alias{nwsVariable}
\description{
  \code{nwsVariable} creates a variable in an R workspace that mirrors
  a variable in a netWorkSpace.  This allows standard R operations
  (get, assign) to be used to share data between R programs running on
  different machines.
}
\usage{
  \S4method{nwsVariable}{netWorkSpace}(.Object, xName, mode=c('fifo', 'lifo','multi','single'),
     env=parent.frame(), force=FALSE, quietly=FALSE)
}
\arguments{
  \item{.Object}{a netWorkSpace class object.}
  \item{xName}{name of variable to be declared.}
  \item{mode}{mode of the variable, see details.}
  \item{env}{environment in which to define active binding.}
  \item{force}{logical; if \code{TRUE}, an existing binding will be overwritten.}
  \item{quietly}{logical; if \code{TRUE}, no warnings are issued.}
}
\details{
  \code{nwsVariable} is built on top of the R \code{makeActiveBinding}
  function.  It is experimental, but we have found that it is very
  useful for introducing people to the concept of netWorkSpace
  variables.  It's not clear that this API is ever preferable to
  \code{nwsStore}, \code{nwsFetch}, \code{nwsFind} for real programs,
  however.

  The \code{mode} of the variable controls what happens when a variable
  is accessed.  If the \code{mode} is \code{'single'}, then all accesses use
  the \code{nwsFind} operation.  If the \code{mode} is \code{'fifo'}, 
  \code{'lifo'}, or \code{'multi'}, then all accesses use the 
  \code{nwsFetch} operation.  
  Assigning a value to an \code{nwsVariable} always uses the
  \code{nwsStore} operation.
}
\examples{
  \dontrun{
# create a netWorkSpace
ws = netWorkSpace('nws example')

# create a variable in the local R workspace that is linked to
# a netWorkSpace variable
nwsVariable(ws, 'x', 'single')

x <- 0
x <- 999  # overwrites the 0
x <- 3.14159  # overwrites the 999
x   # returns 3.14159
x   # returns 3.14159
x   # returns 3.14159

# create a 'fifo' mode variable
nwsVariable(ws, 'message', 'fifo')

message <- 1
message <- 2
message <- 3
message  # returns 1
message  # returns 2
message  # returns 3
  }
}
\keyword{methods}
