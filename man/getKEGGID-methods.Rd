\name{getKEGGID-methods}
\docType{methods}
\alias{getKEGGID}
\alias{getKEGGID-methods}
\alias{getKEGGID,KEGGNode-method}
\title{Get KEGG ID}
\description{
  Get KEGGID from a \code{\link{KEGGNode-class}} object.

  The \code{\link{KEGGNode-class}} can be either another pathway (KEGGID
  in the form like 'hsa\\d*'), KEGG Gene ('hsa:\\d*') or compound
  ('cpd:C\\d*'). In case of the KEGG Gene ID, the organism prefix is
  removed when the value is returned.
}
\section{Methods}{
\describe{
\item{object = "KEGGNode"}{ An object of \code{\link{KEGGNode-class}} }
}}
\examples{
wntfile <- system.file("extdata/hsa04310.xml",package="KEGGgraph")
wnt <- parseKGML(wntfile)
nodes <- nodes(wnt)
getKEGGID(nodes[[1]])
getKEGGID(nodes[[26]])
}
\keyword{methods}

