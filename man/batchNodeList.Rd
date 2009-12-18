\name{batchNodeList}
\alias{batchNodeList}
\alias{sgeNodeList}
\alias{lsfNodeList}
\alias{pbsNodeList}
\title{NodeList Functions}
\description{
Return the list of nodes that SGE, LSF, or PBS has allocated
for our process.
}

\usage{
batchNodeList()
sgeNodeList()
lsfNodeList()
pbsNodeList()
}

\value{
A character vector to pass to sleigh via the nodeList argument.
}

\details{
These functions should only be called from an R program that has been
submitted as a parallel batch job by SGE, LSF, or PBS/Torque.
The batchNodeList function calls either sgeNodeList, lsfNodeList, or
pbsNodeList depending on what environment variables are defined.
The resulting list should be passed to the sleigh function via the
nodeList argument.
}

\seealso{\code{\link{sleigh}}}

\examples{
Sys.setenv(LSB_HOSTS="node1 node2 node3")
batchNodeList()
}

\keyword{utilities}
