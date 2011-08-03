\name{parseEntry}
\alias{parseEntry}
\title{ Parse ENTRY elements of KGML document}
\description{
  ENTRY elements contain information of nodes (proteins, enzymes,
  compounds, maps, etc) in KEGG pathways. 'parseEntry' function parses
  the elements into \code{link{KEGGNode-class}} or \code{\link{KEGGGroup-class}}
  objects. It is not expected to be called directly by the user.
}
\usage{
parseEntry(entry)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{entry}{ XML node of KGML file }
}
\details{
  See \url{http://www.genome.jp/kegg/docs/xml/} for more details about
  'entry' as well as other elements in KGML files.
}
\value{
  An object of \code{link{KEGGNode}} or (in case of a group node) \code{link{KEGGGroup}}
}
\references{\url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang <jitao_david.zhang@roche.com> }
\seealso{ \code{\link{parseGraphics}}, \code{\link{parseKGML}},
  \code{\link{KEGGNode-class}}, \code{\link{KEGGGroup-class}} }
\keyword{ IO }
