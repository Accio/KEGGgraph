\name{randomSubGraph}
\alias{randomSubGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Randomly subset the given graph}
\description{
  The function is intended to be a test tool. It subset the given graph
  repeatedly.
}
\usage{
randomSubGraph(graph, per = 0.25, N = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{ A graph object }
  \item{per}{ numeric, the percentage of the nodes to be sampled, value
    between (0,1) }
  \item{N}{ Repeat times}
}
\value{
  The function is called for its side effect, \code{NULL} is returned
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\examples{
tnodes <- c("Hamburg","Dortmund","Bremen", "Paris")
tedges <- list("Hamburg"=c("Dortmund", "Bremen"),
"Dortmund"=c("Hamburg"), "Bremen"=c("Hamburg"), "Paris"=c())
tgraph <- new("graphNEL", nodes = tnodes, edgeL = tedges)
randomSubGraph(tgraph, 0.5, 10)
}
