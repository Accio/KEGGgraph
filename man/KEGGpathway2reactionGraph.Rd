\name{KEGGpathway2reactionGraph}
\alias{KEGGpathway2reactionGraph}
\title{ Convert chemical reaction network of KEGG pathway into graph}
\description{
  Regulatory pathways are always viewed as protein networks, so there is
  no 'reaction' information saved in their KGML files. Metabolic
  pathways are viewed both as both protein networks and chemical
  networks, hence the \code{\link{KEGGPathway-class}} object may have reactions
  information among chemical compounds.

  This functions extracts reaction information from KEGG pathway, and
  convert the chemical compound reaction network into directed graph.
}
\usage{
KEGGpathway2reactionGraph(pathway)
}
\arguments{
  \item{pathway}{A \code{\link{KEGGPathway-class}} object, usually as the
  result of the function \code{\link{parseKGML}}}
}
\details{
  The direction of the graph is specified by the role of the compound in
  the reaction, the edges goes always out of 'substrate' and points to
  'product'. 

  For now there is no wrapper to parse the KGML file directly into a
  reaction graph. In future there maybe one, but we don't want to
  confuse users with two similar functions to parse the file into a
  graph (since we assume that most users will need the protein graph,
  which can be conveniently parsed by \code{\link{parseKGML2Graph}}).

  From version 1.18.0, reaction graphs returned by
  \code{KEGGpathway2reactionGraph} can be merged with other reaction
  graphs or pathway graphs.Thus users can combine pathway and reaction
  graph in one KGML file into a single graph.
}
\value{
  A directed graph with compounds as nodes and
  reactions as edges.
}
\references{ KGML Document manual \url{http://www.genome.jp/kegg/docs/xml/} }
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\seealso{\code{\link{KEGGPathway-class}}}
\examples{
mapfile <-  system.file("extdata/map00260.xml",package="KEGGgraph")
map <- parseKGML(mapfile)
cg <- KEGGpathway2reactionGraph(map)
cg
nodes(cg)[1:3]
edges(cg)[1:3]
}

