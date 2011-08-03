\name{KEGGpathway2Graph}
\alias{KEGGpathway2Graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Parses KEGGpathway to graph }
\description{
  The function parses an object of \code{\link{KEGGPathway-class}} into
  graph.
}
\usage{
KEGGpathway2Graph(pathway, genesOnly = TRUE, expandGenes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pathway}{ An instance of \code{\link{KEGGPathway-class}} }
  \item{genesOnly}{ logical, should only the genes are maintained and
    other types of nodes (compounds, etc) neglected? TRUE by default}
  \item{expandGenes}{ logical, should homologue proteins expanded? TRUE
    by default }
}
\details{
  When 'expandGenes=TRUE', the nodes have unique names of KEGGID (in the form of 'org:xxxx', for example
  'hsa:1432'), otherwise an auto-increment index given by KEGG is used
  as node names. In the latter case, the node names are duplicated and
  graphs cannot be simply merged before the nodes are unique.

  KEGG node and edge data is stored in 'nodeData' and 'edgeData' slots
  respectively, which can be extracted by \code{\link{getKEGGnodeData}}
  and \code{\link{getKEGGedgeData}}.
}
\value{
  A directed graph.
}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\seealso{ \code{\link{parseKGML2Graph}} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
kegg.pathway <- parseKGML(sfile)
gR.compact<- KEGGpathway2Graph(kegg.pathway,expandGenes=FALSE)
}
