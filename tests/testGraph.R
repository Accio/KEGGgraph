library(KEGGgraph)
library(graph)
library(Rgraphviz)
library(RUnit)

## If edges of a node is not explicitly specified in a directed graph (i.e. the node "D" in the graph object 'test' in the following example),
## its incoming edges will be interpreted as reciprocal edges ( hence inEdges(test)[["B"]]==c("A","D") )
test.nodes <- LETTERS[1:4];
test.edges <- list(A=c("B","C"), B=c("C","D"), C=c("A","D"));
test <- new("graphNEL", nodes=test.nodes, edgeL=test.edges, edgemode="directed");
stopifnot(is.null(adj(test, "D")[[1]]))
stopifnot(inEdges(test)[["B"]]==c("A","D"))

## so if we don't want the side effect above, we have to specify the edges of a no-out-edge node to c() or NULL
test.edges2 <- list(A=c("B","C"), B=c("C","D"), C=c("A","D"), D=NULL);
test2 <- new("graphNEL", nodes=test.nodes, edgeL=test.edges2, edgemode="directed");
inEdges(test2)
stopifnot(adj(test2, "C")[["C"]] == adj(test, "C")[["C"]])
stopifnot(length(adj(test2,"D")[["D"]]) == 0)
stopifnot(inEdges(test2)[["B"]]=="A")

## DO NOT RUN -- it leads to segfault
# plot(test)
## test2 without problem
plot(test2)

## question: a bug or intended so?

##----------------------------------------##
## all edges set to NULL -- disabled!
##----------------------------------------##
test.edge3 <- list(A=NULL, B=NULL, C=NULL, D=NULL)
tryres <- try(test3 <- new("graphNEL", nodes=test.nodes, edgeL=test.edge3, edgemode="directed"))

test.edge4 <- list(A=list(),  B=NULL, C=NULL, D=NULL)
test4 <- new("graphNEL", nodes=test.nodes, edgeL=test.edge4, edgemode="directed")
adj(test4, "A")
inEdges(test4)

test.edge5 <- list(A=c("B","C"),  B=c("A","C"), C=character(0), D=character(0))
test5 <- new("graphNEL", nodes=test.nodes, edgeL=test.edge5, edgemode="directed")
adj(test5, "B")
inEdges(test5)

##----------------------------------------##
## edges must be unique, otherwise an implicit bug will apper
##----------------------------------------##
test.edge6 <- list(A=c("B","B","C"), B=c("A","C"),C=character(0), D=c("A"))
test6 <- new("graphNEL", nodes=test.nodes, edgeL=test.edge6, edgemode="directed")
tryerr <- try(randomSubGraph(test6))

##----------------------------------------##
## test known well-formed graph
##----------------------------------------##
#allpackages <- .packages(all=TRUE)
#if("keggorthology" %in% allpackages) {
#  library(keggorthology)
#  data(KOgraph)
#  randomSubGraph(KOgraph)
#}
