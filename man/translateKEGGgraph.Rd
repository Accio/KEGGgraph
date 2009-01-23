\name{translateKEGGgraph}
\alias{translateKEGGgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Tranlate the KEGG graph from being indexed by KEGGID to another
  identifer }
\description{
  The function translates the KEGG graph into a graph of equivalant
  topology while index with unique identifiers given by user. The new
  identifiers could be, for example, GeneSymbol or other identifiers
  mapped to KEGGID. 
}
\usage{
translateKEGGgraph(graph, newNodes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{ A KEGG graph }
  \item{newNodes}{ A character vector giving the new nodes, must be of
    the same length and same order of the nodes of the given graph }
}
\details{
  The function is still experimental and users are welcomed to report
  any difficulties
}
\value{
  Another graph indexed by the given identifier
}
\author{ Jitao David Zhang <j.zhang@dkfz.de> }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
gR <- parseKGML2Graph(sfile,expandGenes=TRUE)

subG <- subKEGGgraph(c("hsa:1848","hsa:1432","hsa:2002","hsa:8986"),gR)
symbols <- c("DUSP6","MAPK14","ELK1","RPS6KA4")
sub2G <- translateKEGGgraph(subG, symbols)
sub2G
nodes(sub2G)

if(require(Rgraphviz) & interactive()) {
plot(sub2G, "neato")
}
}
