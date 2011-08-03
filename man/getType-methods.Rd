\name{getType-methods}
\docType{methods}
\alias{getType}
\alias{getType-methods}
\alias{getType,KEGGEdge-method}
\alias{getType,KEGGNode-method}
\alias{getType,KEGGReaction-method}
\title{Get type attribute }
\description{
  This method can be used to extract generic type attribute from several
  objects implemented in KEGGgraph package.

  The meanings and descriptions of the types can be found at KGML manual listed in
  the reference.
}
\references{ KGML Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\section{Methods}{
\describe{
  \item{object = "KEGGEdge"}{ An object of \code{\link{KEGGEdge-class}} }
  \item{object = "KEGGNode"}{ An object of \code{\link{KEGGNode-class}} }
  \item{object = "KEGGReaction"}{An object of
    \code{\link{KEGGReaction-class}} }
}}
\examples{
mapfile <- system.file("extdata/map00260.xml", package="KEGGgraph")
maptest <- parseKGML(mapfile)

## node type
node <- nodes(maptest)[[3]]
getType(node)

## edge type
edge <- edges(maptest)[[5]]
getType(edge)

## reaction type
reaction <- getReactions(maptest)[[5]]
getType(reaction)
}
\keyword{methods}

