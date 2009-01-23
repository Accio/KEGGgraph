\name{queryKEGGsubgraph}
\alias{queryKEGGsubgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Query the subgraph of a given KEGG graph with Entrez GeneID (s) }
\description{
  Given a list of genes (identified by Entrez GeneID), the function
  subsets the given KEGG gragh of the genes as nodes (and maintaining
  all the edges between).
}
\usage{
queryKEGGsubgraph(geneids, graph, organism = "hsa", addmissing = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{geneids}{ A vector of Entrez GeneIDs }
  \item{graph}{ A KEGG graph }
  \item{organism}{ a three-alphabet code of organism }
  \item{addmissing}{ logical, in case the given gene is not found in the
  graph, should it be added as single node to the subgraph?}
}
\details{
  This function solves the questions like 'How is the list of gene
  interact with each other in the context of pathways?'

  Limited by the \code{\link{translateKEGGID2GeneID}}, this function
  supports only human for now. We are working to include other
  organisms.

  If 'addmissing' is set to \code{TRUE}, the missing gene in the given
  list will be added to the returned subgraph as single nodes.
}
\value{
  A subgraph with nodes representing genes and edges representing interactions.
}
\author{ Jitao David Zhang <j.zhang@dkfz.de> }
\seealso{ \code{\link{translateGeneID2KEGGID}} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
gR <- parseKGML2Graph(sfile,expandGenes=TRUE)
geneids <- c(5594, 5595, 6197, 5603, 1843,5530, 5603)
sub <- queryKEGGsubgraph(geneids, gR)
if(require(Rgraphviz) && interactive()) {
  plot(sub, "neato")
}

## add missing nodes
list2 <- c(geneids, 81029)
sub2 <- queryKEGGsubgraph(list2, gR,addmissing=TRUE)
if(require(Rgraphviz) && interactive()) {
  plot(sub2, "neato")
}
}
