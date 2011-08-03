\name{getName-methods}
\docType{methods}
\alias{getName}
\alias{getName-methods}
\alias{getName,KEGGEdgeSubtype-method}
\alias{getName,KEGGNode-method}
\alias{getName,KEGGPathwayInfo-method}
\title{ Get 'name' attribute}
\description{
  Get 'name' attribute for given object, this method can be used for
  almost all objects implemented in KEGGgraph package to extract their
  name slot. See manual pages of individual objects for examples.
}
\section{Methods}{
\describe{
\item{object = "KEGGEdgeSubType"}{ An object of \code{\link{KEGGEdgeSubType-class}} }
\item{object = "KEGGNode"}{ An object of \code{\link{KEGGNode-class}} }
\item{object = "KEGGPathway"}{ An object of \code{\link{KEGGPathway-class}} }
\item{object = "KEGGPathwayInfo"}{ An object of
  \code{\link{KEGGPathwayInfo-class}} }
\item{object = "KEGGReaction"}{ An object of
  \code{\link{KEGGReaction-class}} }

}}

\references{ KGML Document Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
pathway <- parseKGML(sfile)

## get pathway name
getName(pathway)

## get node name
nodes <- nodes(pathway)
getName(nodes[[2]])

## get edge name: it is not informative since the nodes are identified
## with file-dependent indices
edges <- edges(pathway)
getName(edges[[7]])

## get subtype name
subtype <- getSubtype(edges[[2]])[[1]]
getName(subtype)
}
\keyword{methods}
