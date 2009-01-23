\name{translateKEGGID2GeneID}
\alias{translateKEGGID2GeneID}
\alias{translateKEGG2GeneID}
\alias{translateGeneID2KEGGID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Translate between KEGGID and Entrez Gene ID}
\description{
  \code{translateKEGGID2GeneID} translates KEGGID to NCBI Entrez Gene ID, and
  \code{translateGeneID2KEGGID} translates Entrez Gene ID back to KEGGID.
}
\usage{
translateKEGGID2GeneID(x, organism="hsa")
translateGeneID2KEGGID(x, organism="hsa")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ KEGGID, e.g. 'hsa:1432', or Entrez Gene ID, e.g. '1432' }
  \item{organism}{ Three alphabet code for organisms. The mapping
    between the orgniams and codes can be found at \url{http://www.genome.jp/kegg/kegg3.html}}
}
\details{
  The KEGGID are unique identifiers used by KEGG PATHWAY to identify
  gene products. After parsing the KEGG pathway into graph, the graph
  use KEGGID as its nodes' names.
  
  \code{translateKEGGID2GeneID} converts KEGGIDs into entrez GeneID, which can be
  translated to other types of identifiers, for example with biomaRt
  package or organism-specific annotation packages. See vignette for
  examples.

  \code{translateKEGG2GeneID} is maintained for back-compatibility and
  wrapps \code{translateKEGGID2GeneID}.
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
translateKEGGID2GeneID(egNodes)
translateGeneID2KEGGID("1432")
}
