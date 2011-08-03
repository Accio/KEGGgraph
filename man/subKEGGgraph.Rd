\name{subKEGGgraph}
\alias{subKEGGgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Subset KEGG graph, including subsetting node and edge attributes}
\description{
  \code{subKEGGgraph} extends generic method \code{subGraph} and subsets the KEGG
  graph. Both 'subKEGGgraph' and 'subGraph' can be used to subset the
  graph, the difference lies in whether the node and edge attributes
  from KEGG are also subset (\code{subKEGGgraph}) or not
  (\code{subGraph}).

  See details below.
}
\usage{
subKEGGgraph(nodes, graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nodes}{Node names to subset}
  \item{graph}{A graph parsed from KGML files, produced by
  \code{\link{parseKGML2Graph}}, \code{\link{KEGGpathway2Graph}} or \code{\link{parseKGMLexpandMaps}}}
}
\details{
  \code{subGraph} does not subset the
  node or edge attributes, hence the results of
  \code{\link{getKEGGnodeData}} and \code{\link{getKEGGedgeData}} does
  not map to the nodes and edges in the subgraph in a one-to-one
  manner, with attributes of removed nodes and edges still remaining in
  the subGraph.

  \code{subKEGGgraph} calls \code{subGraph} first to subset the graph,
  and then it also subsets the KEGGnodeData and KEGGedgeData so that
  they are one-to-one mapped to the nodes and edges in the subgraph. 
}
\value{
  A graph with nodeData and edgeData.
}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
gR <- parseKGML2Graph(sfile,expandGenes=TRUE)
subs <-  c("hsa:1432",edges(gR)$`hsa:1432`,"hsa:5778","hsa:5801",
"hsa:84867","hsa:11072","hsa:5606","hsa:5608","hsa:5494","hsa:5609")
gR.keggsub <- subKEGGgraph(subs, gR)
gR
gR.keggsub
}
