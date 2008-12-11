\name{subtypeDisplay-methods}
\docType{methods}
\alias{subtypeDisplay}
\alias{subtypeDisplay-methods}
\alias{subtypeDisplay,graph-method}
\alias{subtypeDisplay,KEGGEdge-method}
\alias{subtypeDisplay,KEGGEdgeSubType-method}
\title{ Get display information for relation subtypes}
\description{
  To render KEGG pathway graphs, we have created a custom style of edges
  to represent their subtypes. 'subtypeDisplay' extracts this information
}
\section{Methods}{
\describe{

\item{object = "graph"}{ An KEGG graph }

\item{object = "KEGGEdge"}{ An object of \code{\link{KEGGEdge-class}} }

\item{object = "KEGGEdgeSubType"}{  An object of
  \code{\link{KEGGEdgeSubType-class}}  }
}}
\keyword{methods}

