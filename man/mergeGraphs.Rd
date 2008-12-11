\name{mergeGraphs}
\alias{mergeGraphs}
\title{ A function to merge KEGG graphs}
\description{
  The function merges a list of KEGG graphs into one graph object. The
  merged graph have unique nodes, and edges are merged into
  non-duplicate sets.

  For the reason of speed, \code{mergeGraphs} discards KEGG node and
  edge informations. To maintain them while merging graphs, please use \code{\link{mergeKEGGgraphs}}.
}
\usage{
mergeGraphs(list, edgemode = "directed")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{ A list of graph objects, which can be created by \code{\link{parseKGML2Graph}} }
  \item{edgemode}{ Edge mode of the graph product, by default 'directed' }
}
\details{
  The function takes a list of graphs and merges them into a new
  graph. The nodes of individual graphs must be unique. The function
  takes care of the removal of duplicated edges.
}
\value{
  A directed graph
}
\author{ Jitao David Zhang <j.zhang@dkfz.de> }
\note{
    It is known that graphs from C.elegance pathways have
    problem when merging, because the nodes name are not consistent betweeen
    edge records and entry IDs.
}
\seealso{ \code{\link{parseKGML2Graph}} }

