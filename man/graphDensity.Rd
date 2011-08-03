\name{graphDensity}
\alias{graphDensity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Graph density}
\description{
  The graph density is defined as
  d = E/(V*(V-1)/2)
  where E is the number of edges and V of nodes.
}
\usage{
graphDensity(graph)
}
\arguments{
  \item{graph}{ A graph object}
}
\details{
  The density of a graph lies between [0,1]
}
\value{
  A value between [0,1]
}
\references{ Aittokallio and Schwikowski (2006), Graph-based methods for
  analysing networks in cell biology, Briefings in Bioinformatics, 7, 243-255.}
\author{ Jitao David Zhang \url{jitao_david.zhang@roche.com} }
\examples{
tnodes <- c("Hamburg","Dortmund","Bremen", "Paris")
tedges <- list("Hamburg"=c("Dortmund", "Bremen"),
"Dortmund"=c("Hamburg"), "Bremen"=c("Hamburg"), "Paris"=c())
tgraph <- new("graphNEL", nodes = tnodes, edgeL = tedges)
graphDensity(tgraph)
}
