\name{KEGGReaction-class}
\docType{class}
\alias{KEGGReaction-class}
\alias{show,KEGGReaction-method}
\alias{getName,KEGGReaction-method}
\alias{getSubstrate}
\alias{getSubstrate,KEGGReaction-method}
\alias{getProduct}
\alias{getProduct,KEGGReaction-method}
\title{Class "KEGGReaction"}
\description{ A class to present 'reaction' elements in KGML files}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{\link{parseReaction}}.
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character"} the KEGGID of
    this reaction, e.g. "rn:R02749"}
    \item{\code{type}:}{Object of class \code{"character"} the type of
      this reaction, either 'reversible' or 'irreversible'}
    \item{\code{substrateName}:}{Object of class \code{"character"},
      KEGG identifier of the COMPOUND database or the GLYCAN database
      e.g. "cpd:C05378"}
    \item{\code{substrateAltName}:}{Object of class \code{"character"}
      alternative name of its parent substrate element }
    \item{\code{productName}:}{Object of class \code{"character"}
      specifies the KEGGID of the product}
    \item{\code{productAltName}:}{Object of class \code{"character"}
      alternative name of its parent product element }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "KEGGReaction")}: show method}
    \item{getName}{\code{signature(object = "KEGGReaction")}: get the
      KEGGID of the reaction}
    \item{getType}{\code{signature(object = "KEGGReaction")}: get the
      type of the reaction}
    \item{getSubstrate}{\code{signature(object = "KEGGReaction")}: get the
      name of substrate}
    \item{getProduct}{\code{signature(object = "KEGGReaction")}: get the
      name of product}
  }
}
\references{ KGML Document Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\examples{
## We show how to extract reactions from a 'KEGGPathway' object
mapfile <- system.file("extdata/map00260.xml", package="KEGGgraph")

maptest <- parseKGML(mapfile)
mapReactions <- getReactions(maptest)

## More details about reaction
reaction <- mapReactions[[1]]
getName(reaction)
getType(reaction)
getSubstrate(reaction)
getProduct(reaction)
}
\keyword{classes}
