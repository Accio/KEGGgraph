\name{getReactions-methods}
\docType{methods}
\alias{getReactions}
\alias{getReactions-methods}
\alias{getReactions,KEGGPathway-method}
\title{Get KEGG reactions}
\description{
  In KGML, the pathway element specifies one graph object with the \emph{entry}
  elements as its nodes and the \emph{relation} and \emph{reaction} elements as its
  edges. The \emph{relation} elements are saved as \emph{edges} in objects of
  \code{\link{KEGGPathway-class}}, and the \emph{reactions} elements are
  saved as a slot of the object, which can be retrieved with the
  function \code{getReactions}.

  Regulatory pathways are always viewed as protein networks, so there is
  no 'reaction' information saved in their KGML files. Metabolic
  pathways are viewed both as both protein networks and chemical
  networks, hence the \code{KEGGPathway-class} object may have reactions
  information.
}
\section{Methods}{
\describe{
\item{object = "KEGGPathway"}{ An object of \code{\link{KEGGPathway-class}}}
}}
\references{ KGML Document manual \url{http://www.genome.jp/kegg/docs/xml/} }
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\seealso{\code{\link{KEGGPathway-class}}}
\examples{
mapfile <- system.file("extdata/map00260.xml", package="KEGGgraph")
maptest <- parseKGML(mapfile)
maptest

mapReactions <- getReactions(maptest)
mapReactions[1:3]
}
\keyword{methods}

