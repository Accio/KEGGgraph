\name{isHomoList}
\alias{isHomoList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Determines whether a list is homogenous }
\description{
  If a list contains objects of the same class with the given class name, we call it a homogenous
  list and the function returns \code{TRUE}, otherwise it returns \code{FALSE}.
}
\usage{
isHomoList(list, class)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{ A list }
  \item{class}{ The class name to be validated}
}
\value{
  logical
}
\author{ Jitao David Zhang \url{mailto:j.zhang@dkfz.de} }
\examples{
testlist <- list("home1"="Hamburg","home2"="Heidelberg",
"home3"="Tianjin")
isHomoList(testlist,"character")
testlist$lucky <- 16
isHomoList(testlist,"character")
}

