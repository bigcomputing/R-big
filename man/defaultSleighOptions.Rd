\name{defaultSleighOptions}
\alias{defaultSleighOptions}
\title{Default Sleigh Options Environment}
\description{
This environment specifies the default options that are used
when creating a new sleigh object.
}
\seealso{\code{\link{sleigh}}}

\details{
The default options can be modified by assigning new values
to this environment.  Those new values will be used when
constructing all new sleigh objects.  You can also define
new entries, which can be used from custom launch functions.

Note that the \code{defaultSleighOptions} environment is used to
check for illegal options to the sleigh initializer.
Therefore, to allow new options to be passed to a custom
launch function, you must first define a default value for
that option in \code{defaultSleighOptions}.
}

\examples{
defaultSleighOptions$user
}

\keyword{environment}
