\name{userNws}
\docType{methods}
\alias{userNws}
\alias{userNws}
\alias{userNws,sleigh-method}
\title{Get User WorkSpace Created By Sleigh}
\description{
  Get a netWorkSpace object representing the workspace created by the sleigh
}

\usage{
\S4method{userNws}{sleigh}(.Object)
}

\arguments{
  \item{.Object}{a \code{sleigh} class object}
}

\value{
  The user netWorkSpace created by sleigh during construction
}

\examples{
\dontrun{
userNws(sleigh())
}
}


\keyword{methods}
