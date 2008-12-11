\name{getKGMLurl}
\alias{getKGMLurl}
\title{ Get KGML file url with KEGG PATHWAY ID and (optional organism) }
\description{
  The function simply returns the KGML file url given KEGG PATHWAY
  ID. If the KEGG PATHWAY ID contains no organism prefix, user can
  specify the 'organism' parameter. Otherwise the 'organism' option is
  ignored.
}
\usage{
getKGMLurl(pathwayid, organism = "hsa")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pathwayid}{ KEGG PATHWAY ID, e.g. 'hsa00020'}
  \item{organism}{ three-alphabet organism code, if pathwayid contains
  the ocde this option is ignored}
}
\details{
  The function takes the pathway identifier (can be in the form of
  'hsa00020' or with 'pathway' prefix, for example 'path:hsa00020'), and
  returns the url to download KGML file.
}
\value{
  KGML File URL of the given pathway.
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\note{ 
  So far the function does not check the correctness of the
  'organism' prefix, it is the responsibility of the user to garantee
  the right spelling.
}
\examples{
getKGMLurl("hsa00020")
getKGMLurl("path:hsa00020")
getKGMLurl("00020",organism="hsa")
}
\keyword{ IO }
