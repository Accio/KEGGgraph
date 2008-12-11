\name{getPathwayInfo-methods}
\docType{methods}
\alias{getPathwayInfo}
\alias{getPathwayInfo-methods}
\alias{getPathwayInfo,KEGGPathway-method}
\title{ Get KEGG pathway info}
\description{
  KEGG stores additional information of the pathways in their KGML
  files, which can be extracted by this function.

  The method returns the attributes of the pathway including its full
  title, short name, organism, image file link (which can be downloaded
  from KEGG website) and web link.
}
\section{Methods}{
\describe{
\item{object = "KEGGPathway"}{ An object of
  \code{\link{KEGGPathway-class}} }
}}
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
pathway <- parseKGML(sfile)
getPathwayInfo(pathway)
}
\keyword{methods}
