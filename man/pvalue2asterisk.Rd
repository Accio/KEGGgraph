\name{pvalue2asterisk}
\alias{pvalue2asterisk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Return common significance sign (asterisk) associated with given
p value}
\description{
  A p-value of 0.05, 0.01, 0.001 correspond to one, two or three
  asterisks. If 'sig.1' is set to \code{TRUE}, then the p-value of 0.1
  returns '.'.
}
\usage{
pvalue2asterisk(pvalues, sig.1 = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pvalues}{ A numeric value }
  \item{sig.1}{ logical, whether the significance sign of 0.1 should be returned }
}
\value{
  A character string containing the signs
}
\author{ Jitao David Zhang \url{mailto:jitao_david.zhang@roche.com} }
\examples{
pvalue2asterisk(0.03)
pvalue2asterisk(0.007)
pvalue2asterisk(3e-5)
pvalue2asterisk(0.55)
}
