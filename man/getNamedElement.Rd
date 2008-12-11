\name{getNamedElement}
\alias{getNamedElement}
\title{ Extract the value in a vector by name }
\description{
  The function extracts the value(s) in a named vector by given name(s), in
  case no element is found with the given name, NA will be returned
}
\usage{
getNamedElement(vector, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector}{ A named vector of any data type }
  \item{name}{ Wanted name  }
}
\value{
  The elements with the given name, 'NA' in case no one was found
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\examples{
vec <- c(first="Hamburg", second="Hoffenheim",third="Bremen")
getNamedElement(vec, "third")
getNamedElement(vec, "last")
}