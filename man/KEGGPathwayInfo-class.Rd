\name{KEGGPathwayInfo-class}
\docType{class}
\alias{KEGGPathwayInfo-class}
\alias{getTitle,KEGGPathwayInfo-method}
\alias{show,KEGGPathwayInfo-method}
\title{Class "KEGGPathwayInfo" }
\description{ A class to represent information of a KEGG pathway}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{\link{parsePathwayInfo}}.
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character"} Pathway name}
    \item{\code{org}:}{Object of class \code{"character"} Organism }
    \item{\code{number}:}{Object of class \code{"character"} Number }
    \item{\code{title}:}{Object of class \code{"character"} Title of the
    pathway}
    \item{\code{image}:}{Object of class \code{"character"} Image URL }
    \item{\code{link}:}{Object of class \code{"character"} URL Link }
  }
}
\section{Methods}{
  \describe{
    \item{getTitle}{\code{signature(object = "KEGGPathwayInfo")}: get
      title of the pathway}
    \item{show}{\code{signature(object = "KEGGPathwayInfo")}: show method}
    }
}
\references{ KGML Document Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
pathway <- parseKGML(sfile)
pi <- getPathwayInfo(pathway)

class(pi)

getTitle(pi)
}
\keyword{classes}
