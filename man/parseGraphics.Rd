\name{parseGraphics}
\alias{parseGraphics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Parse 'graphics' elements in KGML files}
\description{
  The function parses 'graphics' elements in KGML files, and it is
  mainly used internally.
}
\usage{
parseGraphics(graphics)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graphics}{ XML node}
}
\details{
  The function is called by other parsing functions and not intended to
  be called directly by user.
}
\value{
  An object of \code{\link{KEGGGraphics-class}}.
}
\references{ KGML Document manual \url{http://www.genome.jp/kegg/docs/xml/} }
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\seealso{\code{\link{KEGGGraphics-class}}}