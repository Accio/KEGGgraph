\name{KEGGEdgeSubType-class}
\docType{class}
\alias{KEGGEdgeSubType-class}
\alias{getName,KEGGEdgeSubType-method}
\alias{getValue,KEGGEdgeSubType-method}
\alias{show,KEGGEdgeSubType-method}

\title{Class "KEGGEdgeSubType"}
\description{ A class to represent subtype in KEGG}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("KEGGEdgeSubType", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}, name of the subtype}
    \item{\code{value}:}{Object of class \code{"character"}, value of
      the subtype}
  }
}
\section{Methods}{
  \describe{
    \item{getName}{\code{signature(object = "KEGGEdgeSubType")}: getting
    subtype name}
    \item{getValue}{\code{signature(object = "KEGGEdgeSubType")}:
      getting subtype value}
    \item{show}{\code{signature(object = "KEGGEdgeSubType")}: show method }
  }
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\note{
  Please note that 'KEGGEdgeSubtype' is a data frame storing subtype
  predefinitions, the 'type' with lowercases. 'KEGGEdgeSubType' is
  however a class representing these subtypes.
}
\seealso{
  \code{\link{KEGGEdge-class}}
}
\examples{
showClass("KEGGEdgeSubType")
## use example(KEGGEdge-class) for more examples
}
\keyword{classes}