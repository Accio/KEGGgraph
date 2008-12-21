\name{plotKEGGgraph}
\alias{plotKEGGgraph}
\alias{KEGGgraphLegend}
\title{ Plot KEGG graph with Rgraphviz}
\description{
  The function provides a simple interface to Rgraphviz to render KEGG
  graph with custom styles.

  \code{KEGGgraphLegend} gives the legend of KEGG graphs
}
\usage{
plotKEGGgraph(graph, y = "neato", shortLabel = TRUE, nodeRenderInfo,  ...)
KEGGgraphLegend()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{ A KEGG graph, by calling \code{\link{parseKGML2Graph}} }
  \item{y}{ the layout method, \code{neato} by default }
  \item{shortLabel}{ logical, should be short label used instead of full
    node name?}
  \item{nodeRenderInfo}{ List of node rendering info}
  \item{\dots}{ Other functions passed to renderGraph, not implemented
    for now}
}
\details{
  Users are not restricted to this function, alternatively you can
  choose other rendering functions.
}
\value{
  The graph after layout and rendering is returned.
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\examples{
opar <- par(ask=TRUE)
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
gR <- parseKGML2Graph(sfile,expandGenes=TRUE)
subs <-  c("hsa:1432",edges(gR)$`hsa:1432`,"hsa:5778","hsa:5801","hsa:84867","hsa:11072","hsa:5606","hsa:5608","hsa:5494","hsa:5609")
gR.sub <- subGraph(subs, gR)
if(require(Rgraphviz))
  plotKEGGgraph(gR.sub)

KEGGgraphLegend()
par(opar)
}

