\name{KEGGEdgeSubtype}
\alias{KEGGEdgeType}
\alias{KEGGNodeType}
\alias{KEGGEdgeSubtype}
\docType{data}
\title{ Predefinitions of node or edge types}
\description{
  The KGML files define node and edge type and subtypes, which are
  summarized in these data frames.
}
\usage{
data(KEGGEdgeSubtype)
data(KEGGNodeType)
data(KEGGEdgeType)
}
\format{ They are stored as data frames}
\details{
  They are used by graph render functions to identify different types of
  objects, user could use them to classify edges or nodes.
}
\references{
  KGML Document manual \url{http://www.genome.jp/kegg/docs/xml/}
}
\examples{
data(KEGGEdgeSubtype)
data(KEGGEdgeType)
data(KEGGNodeType)
}
\keyword{datasets}
