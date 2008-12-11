\name{translateKEGG2GeneID}
\alias{translateKEGG2GeneID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Translate KEGGID to Entrez Gene ID}
\description{
  The function translates KEGGID to entrez Gene ID
}
\usage{
translateKEGG2GeneID(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ KEGGID, e.g. 'hsa:1432' }
}
\details{
  The KEGGID are unique identifiers used by KEGG PATHWAY to identify
  gene products. After parsing the KEGG pathway into graph, the graph
  use KEGGID as its nodes' names.
  
  This function converts KEGGIDs into entrez GeneID, which can be translated to other types of identifiers,
  for example with biomaRt package. See vignette for examples.
}
\note{
  This function works so far only with human KEGGIDs, since for them
  the Entrez GeneID can be derived easily with removing the organism
  prefix.

  The complete functional function will be implemented in the later
  release of the package.
}
\value{
  Entrez GeneID of the given KEGG ID(s)
}
\author{ Jitao David Zhang }
\examples{
egNodes <- c("hsa:1432", "hsa:11072")
translateKEGG2GeneID(egNodes)
}
