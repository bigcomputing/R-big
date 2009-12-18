\name{nodeSleighOptions}
\alias{nodeSleighOptions}
\title{Node Sleigh Options Environment}
\description{
This environment specifies node specific options that are used
when creating a new sleigh object.
}

\details{
Node specific options can be specified by assigning list objects
to this environment using a symbol name matches the name of the
node used in the sleigh nodeList argument.
}

\note{
It can be very useful to initialize nodeSleighOptions in an
R startup script, such as .Rprofile.
}

\seealso{\code{\link{sleigh}}, \code{\link{Rprofile}}}

\examples{
\dontrun{
# Use the username 'steve' when starting workers on node 'n1'
nodeSleighOptions$n1 <- list(user = 'steve')

# A more complete example
rrhome <- 'D:/R/R-2.7.0'
fenrir <- list(scriptDir = file.path(rrhome, 'library/nws/bin'),
               rprog = file.path(rrhome, 'bin/Rterm.exe'),
               logDir = 'D:/tmp',
               workingDir = 'D:/tmp',
               python = 'D:/REvolution/Python-2.5.2/python.exe')
nodeSleighOptions$fenrir <- fenrir
}
}

\keyword{environment}
