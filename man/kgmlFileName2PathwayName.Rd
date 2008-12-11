\name{kgmlFileName2PathwayName}
\alias{kgmlFileName2PathwayName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert KGML file name to pathway name }
\description{
  The function uses KEGG package and converts KGML file name into
  human readable pathway name.
}
\usage{
kgmlFileName2PathwayName(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ A KGML file name}
}
\details{
  So far it only supports KGML files organized by species.

  NOTE: there is issue of package loading sequence to use this function:
  the 'KEGG.db' must be loaded before 'KEGGgraph' to use it
  properly. Otherwise the \code{mget} returns error of 'KEGGPATHID2NAME'
  is not a environment. So far I don't where does this bug come from, so
  I commented out the examples.
}
\value{
  A character string of pathway name
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
%\examples{
%library(KEGG.db)
%kgmlFileName2PathwayName("hsa04010.xml")
%}

