\name{getKEGGgeneLink-methods}
\docType{methods}
\alias{getKEGGgeneLink}
\alias{getKEGGgeneLink-methods}
\alias{getKEGGgeneLink,character-method}
\title{ Get KEGG gene link}
\description{
  Tranlsate a object into a link point to the gene on KEGG website.

  This method complies with the Gene link rule of the KEGG website.
  
}
\section{Methods}{
\describe{
\item{object = "character"}{ A KEGGID, for example 'hsa:1423' }
}}
\examples{
getKEGGgeneLink("hsa:1423")
}
\keyword{methods}

