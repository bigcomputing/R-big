\name{getSleigh}
\alias{getSleigh}
\title{Get Sleigh object}
\description{
This function returns the sleigh object registered using
the \code{setSleigh} function.
If a sleigh object hasn't been registered, a default
sleigh will be created and returned.
}
\note{
This function is primarily intended for use in packages that want
to use Sleigh/NetWorkSpaces, but not require their users to explictly
specify sleigh objects.
}
\seealso{\code{\link{setSleigh}}, \code{\link{sleigh}}}

\examples{
\dontrun{
s <- getSleigh()
eachElem(s, sqrt, 1:10)
}
}

\keyword{utilities}
