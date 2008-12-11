\name{KEGGPathway-class}
\docType{class}
\alias{KEGGPathway-class}
\alias{edges,KEGGPathway,ANY-method}
\alias{edges<-}
\alias{edges<-,KEGGPathway-method}
\alias{getName,KEGGPathway-method}
\alias{getTitle,KEGGPathway-method}
\alias{nodes<-,KEGGPathway,ANY-method}
\alias{nodes,KEGGPathway-method}
\alias{edges,KEGGPathway-method}
\alias{show,KEGGPathway-method}
\alias{getTitle,KEGGPathway-method}

\title{Class "KEGGPathway"}
\description{ A class to represent KEGG pathway}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("KEGGPathway",
  ...)} . Normally they are created by \code{\link{parseKGML}}.
}
\section{Slots}{
	 \describe{
    \item{\code{pathwayInfo}:}{ An object of \code{\link{KEGGPathwayInfo-class}}}
    \item{\code{nodes}:}{ List of objects of \code{\link{KEGGNode-class}}}
    \item{\code{edges}:}{  List of objects of
      \code{\link{KEGGEdge-class}} }
    \item{\code{reactions}:}{ List of objects of
      \code{\link{KEGGReaction-class}} }
  }
}
\section{Methods}{
  \describe{
    \item{edges}{\code{signature(object = "KEGGPathway", which =
	"ANY")}: KEGGEdges of the pathway}
    \item{edges<-}{\code{signature(object = "KEGGPathway")}: setting edges}
    \item{getName}{\code{signature(object = "KEGGPathway")}: getting
      pathway name }
    \item{getTitle}{\code{signature(object = "KEGGPathway")}: getting
      pathway title }
    \item{nodes<-}{\code{signature(object = "KEGGPathway", value =
	"ANY")}: setting nodes}
    \item{nodes}{\code{signature(object = "KEGGPathway")}: KEGGNodes of
      the pathway }
    \item{getPathwayInfo}{\code{signature(object = "KEGGPathway")}: getting
    KEGGPathwayInfo}
    \item{getTitle}{\code{signature(object = "KEGGPathway")}: getting
      title of the pathway }
    \item{show}{\code{signature(object = "KEGGPathway")}: display method }
	 }
}
\references{ KGML Document manual \url{http://www.genome.jp/kegg/docs/xml/} }
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }

\seealso{\code{\link{parseKGML}},
  \code{\link{KEGGEdge-class}}, \code{\link{KEGGNode-class}},
  \code{\link{KEGGReaction-class}}
}

\examples{
## We show how to extract information from KEGGPathway objects
## Parse KGML file into a 'KEGGPathway' object
mapfile <- system.file("extdata/map00260.xml", package="KEGGgraph")
maptest <- parseKGML(mapfile)

## short summary of the pathway
maptest

## get information of the pathway
getPathwayInfo(maptest)

## nodes of the pathway
nodes <- nodes(maptest)
node <- nodes[[3]]
getName(node)
getType(node)
getDisplayName(node)

## edges of the pathway
edges <- edges(maptest)
edge <- edges[[3]]
getEntryID(edge)
getSubtype(edge)
}
\keyword{classes}
