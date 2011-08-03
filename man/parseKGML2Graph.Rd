\name{parseKGML2Graph}
\alias{parseKGML2Graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Parse KGML files into KEGG graph }
\description{
  This function is a wrapper for parseKGML and KEGGpathway2Graph. It
  takes two actions: first it reads in the KGML file and parses it into an object of
  \code{\link{KEGGPathway-class}}, the second step it calls
  \code{\link{KEGGpathway2Graph}} function to return the graph model.
}
\usage{
parseKGML2Graph(file, ...)
}
\arguments{
  \item{file}{ Name of KGML file}
  \item{\dots}{ other parameters passed to KEGGpathway2Graph, see
    \code{\link{KEGGpathway2Graph}} }
}
\value{
  A graph object.
}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
gR <- parseKGML2Graph(sfile,expandGenes=TRUE)
gR
}
