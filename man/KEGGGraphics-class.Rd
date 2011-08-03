\name{KEGGGraphics-class}
\docType{class}
\alias{KEGGGraphics-class}

\title{ class 'KEGGGraphics' }
\description{ A class to represent 'graphics' element in KGML files }
\section{Objects from the Class}{
  This method is mainly used to extract visualization information from
  KGML files.

  Objects can be created by calling \code{\link{parseGraphics}}
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character"} graphics name }
    \item{\code{x}:}{Object of class \code{"integer"} x coordinate in
      KEGG figure }
    \item{\code{y}:}{Object of class \code{"integer"} y coordinate in
      KEGG figure }
    \item{\code{type}:}{Object of class \code{"character"} graphics type
    (shape)}
    \item{\code{width}:}{Object of class \code{"integer"} witdh of the symbol }
    \item{\code{height}:}{Object of class \code{"integer"} height of the
    symbol}
    \item{\code{fgcolor}:}{Object of class \code{"character"} foreground
    color}
    \item{\code{bgcolor}:}{Object of class \code{"character"} background
    color}
  }
}
\references{ KGML Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\seealso{
  \code{\link{parseGraphics}}
}
\examples{
showClass("KEGGGraphics")
}
\keyword{classes}
