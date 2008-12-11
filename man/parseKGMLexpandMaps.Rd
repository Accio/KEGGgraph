\name{parseKGMLexpandMaps}
\alias{parseKGMLexpandMaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ A convenient function to parse KGML and expand its containing
  maps into one graph object }
\description{
  The function does several tasks implemented in the KEGGgraph package
  in sequence to make expanding maps easier.
}
\usage{
parseKGMLexpandMaps(file, downloadmethod = "wget", genesOnly = TRUE, localdir,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ A KGML file }
  \item{downloadmethod}{ passed to \code{\link{download.file}} function
    as 'method', see its documentation for more details }
  \item{genesOnly}{ logical, should only the genes nodes remain in the
    returned graph object? }
  \item{localdir}{ character string, if specified, the function tries to
    read files with the same base name from a local directory, useful
    when there are file copies on the client.}
  \item{\dots}{ Other parameters passed to  \code{\link{download.file}} }
}
\details{
  In KEGG pathways there're usually pathways contained('cross-linked')
  in other pathways, for example see
  \url{http://www.genome.jp/kegg/pathway/hsa/hsa04115.html}, where p53
  signalling pathway contains other two pathways 'apoptosis' and 'cell
  cycle'. This function parses these pathways (refered as 'maps' in KGML
  manual), download their KGML files from KEGG FTP website, parse them
  individually, and merge all the children pathway graphs with the
  parental pathway into one graph object. The graph is returned as the
  function value.

  Since different graphs does not have unique node identifiers unless
  the genes are expanded, so by using this function user has to expand
  the genes. Another disadvantage is that so far due to the
  implementation, the KEGGnodeData and KEGGedgeData is lost during the
  merging. This however will probably be changed in the future version.
}
\value{
  A directed graph object
}
\references{KGML Document manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{j.zhang@dkfz.de}}
\seealso{ for most users it is enough to use \code{\link{parseKGML2Graph}} }
