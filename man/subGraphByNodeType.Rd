\name{subGraphByNodeType}
\alias{subGraphByNodeType}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Subset KEGG graph by node types}
\description{
  The function subsets KEGG graph by node types, mostly used in
  extracting gene networks.
}
\usage{
subGraphByNodeType(graph, type = "gene", kegg=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{ A KEGG graph object produced by calling \code{\link{parseKGML2Graph}} }
  \item{type}{ node type, see \code{\link{KEGGNodeType}} for details }
  \item{kegg}{ logical, should the KEGG Node and Edge attributes be
    maintained during the subsetting? By default set to 'TRUE'}
}
\value{
  A subgraph of the original graph
}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
sGraph <- parseKGML2Graph(sfile,expandGenes=TRUE, genesOnly=FALSE)
sGraphGene <- subGraphByNodeType(sGraph, type="gene")
}