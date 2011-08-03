\name{getRgraphvizEdgeNames}
\alias{getRgraphvizEdgeNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Get Rgraphviz compatitable edge names }
\description{
  Get Rgraphviz compatitable edge names, where the out- and in-nodes
  sharing a edge are concatenated by "~".
}
\usage{
getRgraphvizEdgeNames(graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{ A graph object }
}
\value{
  A list of names, the order is determined by the edge order.
}
\references{ Rgraphviz package }
\author{ Jitao David Zhang \url{maito:jitao_david.zhang@roche.com} }
\examples{
tnodes <- c("Hamburg","Dortmund","Bremen", "Paris")
tedges <- list("Hamburg"=c("Dortmund", "Bremen"),
"Dortmund"=c("Hamburg"), "Bremen"=c("Hamburg"), "Paris"=c())
tgraph <- new("graphNEL", nodes = tnodes, edgeL = tedges)
getRgraphvizEdgeNames(tgraph)
}

