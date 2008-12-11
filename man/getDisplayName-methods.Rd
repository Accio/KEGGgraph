\name{getDisplayName-methods}
\docType{methods}
\alias{getDisplayName}
\alias{getDisplayName-methods}
\alias{getDisplayName,KEGGNode-method}
\alias{getDisplayName,graph-method}
\title{ Get a character string as label for display}
\description{
  In KGML files, 'graph' element has a 'name' attribute to store the
  displaying name of a node, which is straighforward for end users. For
  example, biologists have no idea about a node 'hsa:1432' but its
  display name 'MAPK14' helps them to link this node to their knowledge.
  This method extract DisplayName from graph objects for KEGGNode and graph, where the
  method for graph returns the display names of its nodes.
}
\section{Methods}{
  \describe{  
    \item{object = "KEGGNode"}{ An object of \code{\link{KEGGNode-class}} }
    \item{object = "graph"}{ A KEGG graph object}
}}
\references{ KGML Document Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
pathway <- parseKGML(sfile)

nodes <- nodes(pathway)
subnodes <- nodes[10:15]

sapply(subnodes, getDisplayName)
## compare them with getName, one 'displayName' may correspond to many paralogues
sapply(subnodes, getName)

}
\keyword{methods}
