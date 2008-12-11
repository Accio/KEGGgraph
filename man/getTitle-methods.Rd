\name{getTitle-methods}
\docType{methods}
\alias{getTitle}
\alias{getTitle-methods}
\title{ Get title for given element}
\description{
  The methods get title attribute for given KGML element, for example
  for objects of \code{\link{KEGGPathway-class}} or \code{\link{KEGGPathwayInfo-class}}
}
\references{ KGML Document manual \url{http://www.genome.jp/kegg/docs/xml/} }
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\section{Methods}{
\describe{

\item{object = "KEGGPathway"}{ An object of \code{\link{KEGGPathway-class}} }

\item{object = "KEGGPathwayInfo"}{ An object of \code{\link{KEGGPathwayInfo-class}} }
}}
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
pathway <- parseKGML(sfile)

getTitle(pathway)

pi <- getPathwayInfo(pathway)
getTitle(pi)
}

\keyword{methods}

