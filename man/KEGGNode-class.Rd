\name{KEGGNode-class}
\docType{class}
\alias{KEGGNode-class}
\alias{show,KEGGNode-method}
\alias{name<-,KEGGNode-method}
\alias{name<-}
\alias{entryID<-}
\alias{entryID<-,KEGGNode-method}
\alias{getComponent,KEGGNode-method}

\title{Class "KEGGNode"}
\description{ The class to present 'entry' element in KGML files and
  nodes in KEGG graphs}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{\link{parseEntry}}
and is not intended to be directly created by users.
}
\section{Slots}{
	 \describe{
    \item{\code{entryID}:}{ entryID, the 'id' attribute of 'entry'
      elements in KGML files. In each KGML file the entryID is specified
    by auto-increment integers, therefore entryIDs from two individual
    KGML files are not unique. However, if 'expandGenes' option is
    specified in \code{\link{KEGGpathway2Graph}} function, the unique
    KEGGID will replace the default integer as the new entryID, which is
    unique in biological context }
    \item{\code{name}:}{Name of the node }
    \item{\code{type}:}{Type of the node, use \code{data(KEGGNodeType)}
      to see available values }
    \item{\code{link}:}{URL link of the node}
    \item{\code{reaction}:}{Reaction of the node}
    \item{\code{map}:}{Map of the node}
    \item{\code{graphics}:}{Graphic details (including display name) of
      the node, an object of \code{\linkS4class{KEGGGraphics}}}
  }
}
\section{Methods}{
  \describe{
    \item{getDisplayName}{\code{signature(object = "KEGGNode")}: get
      display name }
    \item{getEntryID}{\code{signature(obj = "KEGGNode")}: get entryID,
      in case of gene-expanded graphs this is the same as getKEGGID}
    \item{getKEGGID}{\code{signature(object = "KEGGNode")}: get KEGGID}
    \item{getType}{\code{signature(object = "KEGGNode")}: get the type of
      the node}
    \item{<-name}{\code{signature(object = "KEGGNode")}: replace name }
    \item{getComponent}{\code{signature(obj = "KEGGNode")}: returns
      entryID (the same as \code{getEntryID}), for compatibility with \code{\link{KEGGGroup-class}}}
    \item{show}{\code{signature(object = "KEGGNode")}: show method }
  }
}
\references{ KGML Document manual \url{http://www.genome.jp/kegg/docs/xml/} }
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }

\seealso{\code{\link{KEGGEdge-class}}, \code{\link{parseEntry}}}
\examples{
## We show how to extract information from KEGGNode object
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
pathway <- parseKGML(sfile)

ns <- nodes(pathway)
node <- ns[[1]]

show(node)
getName(node)
getDisplayName(node)
getEntryID(node)
getKEGGID(node)
}
\keyword{classes}
