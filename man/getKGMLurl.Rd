\name{getKGMLurl}
\alias{getKGMLurl}
\alias{retrieveKGML}
\alias{getCategoryIndepKGMLurl}
\alias{kgmlNonmetabolicName2MetabolicName}
\title{ Get KGML file (url) with KEGG PATHWAY ID and (optional) organism }
\description{
  The function simply returns the KGML file url given KEGG PATHWAY
  ID. If the KEGG PATHWAY ID contains no organism prefix, user can
  specify the 'organism' parameter. Otherwise the 'organism' option is
  ignored.

  retrieveKGML is a simple wrapper to getKGMLurl, which downloads the
  KGML file with \code{download.file} in utils package.

  \code{kgmlNonmetabolicName2MetabolicName} is used to translate
  non-metabolic pathway KGML URL to that of metabolic
  pathway. \code{getCategoryIndepKGMLurl} determines the correct URL to
  download by attempting both possibilities. They are mainly called internally.
}
\usage{
getKGMLurl(pathwayid, organism = "hsa")
retrieveKGML(pathwayid, organism, destfile,method="wget", ...)
kgmlNonmetabolicName2MetabolicName(destfile)
getCategoryIndepKGMLurl(pathwayid, organism="hsa", method="wget", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pathwayid}{ KEGG PATHWAY ID, e.g. 'hsa00020'}
  \item{organism}{ three-alphabet organism code, if pathwayid contains
    the ocde this option is ignored}
  \item{destfile}{Destination file, to which the remote KGML file should
    be saved}
  \item{method}{Method to be used for downloading files, passed to \code{download.file} function. Currently supports "internal", "wget" and
    "lynx"}
  \item{...}{Parameters passed to download.file}
}
\details{
  The function \code{getKGMLurl} takes the pathway identifier (can be in the form of
  'hsa00020' or with 'pathway' prefix, for example 'path:hsa00020'), and
  returns the url to download KGML file.

  The mapping between pathway identifier and pathway name can be found
  by KEGGPATHNAME2ID (or reversed mappings) in KEGG.db package. See
  vignette for example.
  
  \code{retrieveKGML} calls \code{download.file} to download the KGML
  file from KEGG FTP remotely. 
}
\value{
  KGML File URL of the given pathway.
}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\note{ 
  So far the function does not check the correctness of the
  'organism' prefix, it is the responsibility of the user to garantee
  the right spelling.

  For Windows users, it is necessary to download and install \code{wget} program
  (\url{http://gnuwin32.sourceforge.net/packages/wget.htm}) to use the
  \code{wget} method to download files. Sometimes it may be necessary to
  modify searching path to add GnuWin32 folder (where \code{wget}
  execution file is located) and re-install R to make
  \code{wget} work.

  Some user may face difficulty
  of retrieving KGML files when the download method is set to
  \sQuote{auto}. In this case setting the method to \sQuote{wget} may
  solve the problem (thanks to the report by Gilbert Feng).
}
\examples{
getKGMLurl("hsa00020")
getKGMLurl("path:hsa00020")
getKGMLurl("00020",organism="hsa")

## NOT RUN
tmp <- tempfile()
retrieveKGML(pathwayid='00010', organism='cel', destfile=tmp, method="wget")
}
\keyword{ IO }
