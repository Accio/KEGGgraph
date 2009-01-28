\name{neighborhood}
\alias{neighborhood}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Return the neighborhood set of given vertices}
\description{
  The function returns the neighborhood set of given vertices in the
  form of list. Optionally user can choose to include the given vertices
  in the list, too.
}
\usage{
neighborhood(graph, index, return.self = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{ An object of \code{graphNEL}}
  \item{index}{ Names of nodes, whose neighborhood set should be returned }
  \item{return.self}{ logical, should the vertex itself also be returned?}
}
\details{
  Let _v_ be a vertex in a (di)graph, the out-neighborhood or
  successor set (N+(v), x belongs to V(G) and v->x) and the
  in-neighborhood or predecessor set (N-(v), x belongs to V(G) and x->v)
  are jointly returned.

  The returned list is indexed by the given node indices, \code{NULL} is
  returned in case of non-existing node.

  The nodes are unique, that is, duplicated nodes are removed in results.
}
\value{
  A list indexed by the given node indices, each entry containing the
  neighborhood set of that node (or furthermore including that node).
}
\references{D.B. West. Introduction to Graph Theory, Second Edition.
  Prentice Hall, 2001}
\author{ Jitao David Zhang <j.zhang@dkfz.de> }
\examples{
  V <- c("Hamburg","Stuttgart","Berlin","Paris","Bremen")
  E <- list("Hamburg"=c("Berlin","Bremen"),
            "Stuttgart"=c("Berlin","Paris"),
            "Berlin"=c("Stuttgart","Bremen"),
            "Paris"=c("Stuttgart"),
            "Bremen"=c("Hamburg","Berlin"))
  g <- new("graphNEL", nodes=V, edgeL=E, edgemode="directed")
  if(require(Rgraphviz) & interactive()) {
    plot(g, "neato")
  }

  ## simple uses
  neighborhood(g, "Hamburg")
  neighborhood(g, c("Hamburg", "Berlin","Paris"))

  ## in case of non-existing nodes
  neighborhood(g, c("Stuttgart","Ulm"))

  ## also applicable to non-directed graphs
  neighborhood(ugraph(g), c("Stuttgart","Berlin"))
}
