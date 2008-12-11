\name{parseReaction}
\alias{parseReaction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Parse reaction from KGML files}
\description{
  The function parses 'reaction' element in KGML files. It is used
  interally and not expected to be called by users.
}
\usage{
parseReaction(reaction)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reaction}{ A node of the type 'reaction' in KGML files}
}
\details{
  See the reference manual for more information about 'reaction' type
}
\value{
  An object of \code{\link{KEGGReaction-class}}
}
\references{ KGML Document Manual \url{http://www.genome.jp/kegg/docs/xml/}}
\author{ Jitao David Zhang \url{mail:j.zhang@dkfz.de} }
\keyword{IO}