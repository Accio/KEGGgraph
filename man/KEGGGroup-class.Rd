\name{KEGGGroup-class}
\docType{class}
\alias{KEGGGroup-class}
\alias{getComponent}
\alias{getComponent,KEGGGroup-method}
\title{Class "KEGGGroup" }
\description{ Class to represent 'group' nodes in KEGG pathways}
\section{Objects from the Class}{
  The objects are usually created by \code{\link{parseEntry}} function
  and is not intended to be called directly by users.
}
\section{Slots}{
	 \describe{
	   \item{\code{component}:}{ Component of the group }
	    \item{\code{entryID}:}{ see the slot of
	      \code{\link{KEGGNode-class}} }
	    \item{\code{graphics}:}{ see the slot of
	      \code{\link{KEGGNode-class}}}
	    \item{\code{link}:}{ see the slot of
	      \code{\link{KEGGNode-class}}}
	    \item{\code{map}:}{ see the slot of
	      \code{\link{KEGGNode-class}}}
	    \item{\code{name}:}{ see the slot of
	      \code{\link{KEGGNode-class}}}
	    \item{\code{reaction}:}{ see the slot of
	      \code{\link{KEGGNode-class}}}
	    \item{\code{type}:}{ see the slot of \code{\link{KEGGNode-class}}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{KEGGNode}"}, directly.
}
\section{Methods}{
  \describe{
    \item{getComponent}{\code{signature(object = "KEGGNode")}: returns
      components of the group, in a vector of strings}
  }
}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\seealso{
  \code{\link{KEGGNode-class}}
}
\examples{
showClass("KEGGGroup")
}
\keyword{classes}
