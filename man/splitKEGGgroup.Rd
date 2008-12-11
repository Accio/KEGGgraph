\name{splitKEGGgroup}
\alias{splitKEGGgroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Split KEGG group}
\description{
  The function split 'group' entries in KGML files. Most of the cases
  they are complexes. During the splitting the function copies the edges
  between groups and nodes (or between groups and groups)
  correspondingly, so that the existing edges also exist after the
  groups are split.
}
\usage{
splitKEGGgroup(pathway)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pathway}{ An object of \code{\link{KEGGPathway-class}} }
}
\details{
  By default the groups (complexes) in KEGG pathways are split.
}
\value{
  An object of \code{\link{KEGGPathway-class}} 
}
\references{KGML Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\seealso{ \code{\link{KEGGpathway2Graph}} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
kegg.pathway <- parseKGML(sfile)
kegg.pathway.split <- splitKEGGgroup(kegg.pathway)

## compare the different number of edges
length(edges(kegg.pathway))
length(edges(kegg.pathway.split))
}

