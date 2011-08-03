\name{parsePathwayInfo}
\alias{parsePathwayInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Parse information of the pathway from KGML files}
\description{
  The function parses the information of the given pathway from KGML
  files into an object of \code{\link{KEGGPathwayInfo-class}}. It is
  used internally and is not expected to be called by users directly.
}
\usage{
parsePathwayInfo(root)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{root}{ Root element of the KGML file}
}
\value{
  An object of \code{\link{KEGGPathwayInfo-class}}
}
\references{ KGML Document Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com}}
\keyword{IO}