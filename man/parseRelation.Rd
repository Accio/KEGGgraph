\name{parseRelation}
\alias{parseRelation}
\title{ Parse RELATION elements from KGML files }
\description{
  RELATION elements in KGML files record the binary relationships
  between ENTRY elements, corresponding to (directed) edges in a
  graph. 'parseRelation' function parses RELATION elements into
  \code{\link{KEGGEdge-class}} objects from KGML
  files. It is not expected to be called directly by the user.
}
\usage{
parseRelation(relation)
}
\arguments{
  \item{relation}{ XML node of KGML file }
}
\details{
  See \url{http://www.genome.jp/kegg/docs/xml/} for more details about
  'relation' as well as other elements in KGML files.
}
\value{
  An object of \code{link{KEGGEdge}}.
}
\references{\url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang <j.zhang@dkfz.de> }
\seealso{\code{\link{KEGGEdge-class}}, \code{\link{parseEntry}}}
\keyword{ IO }
