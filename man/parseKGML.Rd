\name{parseKGML}
\alias{parseKGML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ KGML file parser }
\description{
  The function parses KGML files according to the KGML XML documentation.
}
\usage{
parseKGML(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ Name of KGML file}
}
\details{
  The function parses KGML file (depending on XML package).
}
\value{
  An object of \code{\link{KEGGPathway-class}}.
}
\references{ KGML Manual \url{http://www.genome.jp/kegg/docs/xml/} }
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\seealso{
  \code{\link{parseEntry}}, \code{\link{parseRelation}},
  \code{\link{parseReaction}}, \code{\link{KEGGPathway-class}},
}
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
kegg.pathway <- parseKGML(sfile)
kegg.pathway
}
