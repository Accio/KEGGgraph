\name{getEntryID-methods}
\docType{methods}
\alias{getEntryID}
\alias{getEntryID-methods}
\alias{getEntryID,KEGGEdge-method}
\alias{getEntryID,KEGGNode-method}
\alias{getEntryID,list-method}
\title{ Get entry ID for single or list of KEGGNode or KEGGedge object(s)}
\description{
  The method extracts EntryIDs from \code{\link{KEGGNode-class}} or
  \code{\link{KEGGEdge-class}} object(s).

  In case of \code{\link{KEGGEdge-class}} objects, the entryID of the nodes
  involved in the binary are returned as a vector \emph{in the order
    specified by the direction of the relation}, that is, if the
  edge is defined as A->B, then the entryID returned from the edge
  equals to c(getEntryID(A), getEntryID(B)).
}
\section{Methods}{
  \describe{
    \item{obj = "KEGGEdge"}{ Object of \code{\link{KEGGEdge-class}} }
%    \item{obj = "KEGGNode"} { Object of \code{\link{KEGGNode-class}} }
    \item{obj = "list"}{ A wrapper for list of \code{\link{KEGGNode-class}} or
      \code{\link{KEGGEdge-class}} objects}
}}
\references{ KGML Document Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
pathway <- parseKGML(sfile)

nodes <- nodes(pathway)
node <- nodes[[7]]
getEntryID(node)

edges <- edges(pathway)
edge <- edges[[7]]
getEntryID(edge)

getEntryID(nodes[1:4])
getEntryID(edges[1:4])
}
\keyword{methods}
