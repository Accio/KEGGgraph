\name{getKEGGnodeData}
\alias{getKEGGnodeData}
\alias{getKEGGedgeData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Get list of KEGG node or edge data }
\description{
  The methods extracts KEGG node (edge) attributes from a graph produced
  by calling \code{\link{parseKGML2Graph}} or \code{\link{KEGGpathway2Graph}}.
}
\usage{
getKEGGnodeData(graph, n)
getKEGGedgeData(graph, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{ a graph object by parsing KGML file, where KEGG node and
  edge attributes are maintained}
  \item{n}{ optional character string, name of the desired node or
    edge. If is missing all node Data is returned}
}
\details{
  Node and edge data is stored as list within environments in graphs to save
  memory and speed up graph manipulations. When using
  \code{getKEGGnodeData} or \code{getKEGGedgeData} is called, the list
  is extracted out of the environment and returned.
}
\value{
  Either a list or single item of \code{\link{KEGGNode-class}} or
  \code{\link{KEGGEdge-class}} object(s).
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
gR <- parseKGML2Graph(sfile,expandGenes=TRUE)
getKEGGnodeData(gR,"hsa:4214")
getKEGGedgeData(gR,"hsa:4214~hsa:5605")
}
