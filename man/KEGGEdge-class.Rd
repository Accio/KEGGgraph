\name{KEGGEdge-class}
\docType{class}
\alias{KEGGEdge-class}
\alias{show,KEGGEdge-method}
\alias{entryID<-,KEGGEdge-method}
\alias{getName,KEGGEdge-method}
\title{Class 'KEGGEdge' }
\description{ A class to represent 'relation' elements in KGML files and
edge objects in a KEGG graph}
\section{Objects from the Class}{
  Objects are normally created by \code{\link{parseRelation}} function,
  which is not intended to be called by user directly
}
\section{Slots}{
	 \describe{
    \item{\code{entry1ID}:}{ The entryID of the first KEGGNode }
    \item{\code{entry2ID}:}{ The entryID of the second KEGGNode }
    \item{\code{type}:}{ The type of the relation, see \code{\link{getType-methods}} }
    \item{\code{subtype}:}{ The subtype(s) of the edge, a list of \code{\linkS4class{KEGGEdgeSubType}}}
  }
}
\section{Methods}{
  \describe{
    \item{getEntryID}{\code{signature(obj = "KEGGEdge")}: Get entryIDs
      of the edge in the order specified by the direction of the edge }
    \item{getType}{\code{signature(object = "KEGGEdge")}: Get the
      relation type }
    \item{getName}{\code{signature(object = "KEGGEdge")}: Get the names
      of edges in the convention of Rgraphviz, 'node1~node2'}
    \item{show}{\code{signature(object = "KEGGEdge")}: Show method }
	 }
}
\references{ KGML Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\seealso{
  \code{\link{KEGGNode-class}}
}
\examples{
mapfile<- system.file("extdata/map00260.xml", package="KEGGgraph")
maptest <- parseKGML(mapfile)

x <- edges(maptest)[[1]]
class(x)

## examples to extract information from KEGGEdge
getName(x)
getEntryID(x)

getType(x)
getSubtype(x)

subtype <- getSubtype(x)[[1]]
getName(subtype)
}
\keyword{classes}
