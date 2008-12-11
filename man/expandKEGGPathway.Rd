\name{expandKEGGPathway}
\alias{expandKEGGPathway}
\title{ Expand KEGG Pathway }
\description{
  The function expands paralogue nodes in KEGG pathway and returns
  expanded KEGG pathway, KEGG node and edge data is maintained.
}
\usage{
expandKEGGPathway(pathway)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pathway}{ An object of \code{\link{KEGGPathway-class}} }
}
\details{
  The function expands nodes with paralogues in KEGG pathway and copy
  neccessary edges.
}
\value{
  An object of \code{\link{KEGGPathway-class}}
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\seealso{ \code{\link{expandKEGGNode}} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
kegg.pathway <- parseKGML(sfile)
kegg.expandpathway <- expandKEGGPathway(kegg.pathway)
}
