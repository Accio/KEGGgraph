\name{parseKGML2DataFrame}
\alias{parseKGML2DataFrame}
\title{Parse KGML file into a data frame}
\description{
  This function extends the \code{\link{parseKGML2Graph}} function, by
  converting the resulting graph into a three-column data frame
  representing out-nodes (the \code{from} column in the data frame),
  in-nodes (\code{to}) and subtypes of edges that connect
  them (\code{subtype}). It can be used, for example, for exporting KEGG pathway networks
  in plain text files.
}
\usage{
parseKGML2DataFrame(file, reactions=FALSE,...)
}
\arguments{
  \item{file}{A KGML file}
  \item{reactions}{Logical, whether metabolic reactions should be parsed
  and returned as part of the data frame. Default:\code{FALSE}}
  \item{\dots}{Other parameters passed to \code{\link{KEGGpathway2Graph}}}
}
\details{The out- and in-nodes are represented in the form of KEGG
  identifiers. For human EntrezIDs the function
  \code{\link{translateKEGGID2GeneID}} can be used.

  Multile edges are supported: in case more than one subtypes of edges
  exist between two nodes, they are all listed in the resulting data frame.
}
\value{A three-column data frame, representing the graph structure:
  out-nodes (the \code{from} column), in-nodes (\code{to}) and edge
  subtype (\code{subtype}).}
\author{Jitao David Zhang}
\seealso{
\code{\link{parseKGML2Graph}}, \code{\link{KEGGpathway2Graph}} and \code{\link{translateKEGGID2GeneID}}.
}
\examples{
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")
gdf <- parseKGML2DataFrame(sfile)
head(gdf)
dim(gdf)

rfile <- system.file("extdata/hsa00020.xml",package="KEGGgraph")
dim(dfWr <- parseKGML2DataFrame(rfile, reactions=TRUE))
dim(dfWOr <- parseKGML2DataFrame(rfile, reactions=FALSE))
stopifnot(nrow(dfWr)>nrow(dfWOr))

## not expanding genes: only the KGML-specific identifiers are used then
## only for expert use
## NOT RUN
gdf.ne <- parseKGML2DataFrame(sfile, expandGenes=FALSE)
dim(gdf.ne)
head(gdf.ne)
## NOT RUN
}
