\name{mergeKEGGgraphs}
\alias{mergeKEGGgraphs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Merge KEGG graphs, also merging KEGGNode and KEGGEdge attributes}
\description{
  \code{mergeKEGGgraphs} extends function \code{mergeGraphs} and merges
  a list of KEGG graphs. Both \code{mergeGraphs} and
  \code{mergeKEGGgraphs} can be used to merge graphs, while the latter
  form is able to merge the nodes and edges attributes from KEGG, so
  that the nodes and edges have a one-to-one mapping to the results from
  \code{getKEGGnodeData} and \code{getKEGGEdgeData}.

  See details below.
}
\usage{
mergeKEGGgraphs(list, edgemode = "directed")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{ A list of named KEGG graphs}
  \item{edgemode}{ character, 'directed' by default}
}
\details{
  \code{mergeGraphs} discards the node or edge attributes, hence
  \code{\link{getKEGGnodeData}} or \code{\link{getKEGGedgeData}} will
  return \code{NULL} on the resulting graph.

  \code{mergeKEGGgraphs} calls \code{mergeGraphs} first to merge the
  graphs, then it also merges the KEGGnodeData and KEGGedgeData.so that
  they are one-to-one mapped to the nodes and edges in the merged graph.
}
\value{
  A graph with nodeData and edgeData
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\seealso{ \code{\link{mergeGraphs}} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
gR <- parseKGML2Graph(sfile,expandGenes=TRUE)

wntfile <- system.file("extdata/hsa04310.xml",package="KEGGgraph")
wntR <- parseKGML2Graph(wntfile, expandGenes=TRUE)

graphlist <- list(mapkG=gR, wntG=wntR)
mergedKEGG <- mergeKEGGgraphs(graphlist)

mergedKEGG
}

