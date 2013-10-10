# test_KEGGgraph.R
#-------------------------------------------------------------------------------
# move these to paulsTests when development slows
library(KEGGgraph)
library(RUnit)
#-------------------------------------------------------------------------------
# this test demonstrates the difficult encountered by Stuart Bradley
# in which mergeGraphs works with two reaction graphs, but mergeKEGGgraphs
# does not.
# i think the underlying problem is that  mergeKEGGgraphs (in KEGGgraph/R/graph.R)
# requires that there be an edge attribute "KEGGEdge" and a node attribute
# "KEGGNode", and  fails with the error you saw.
#    
#    So the reaction graph does not have the two attributes required by
#    mergeKEGGgraphs.  I am not sure whether this is by design, or
#    whether it is an oversight.
#
test_mergeKEGGgraphs <- function()
{
    dir <- tempdir()
    filename.1 <- file.path(dir, "hsa00260.kgml")
    filename.2 <- file.path(dir, "hsa00020.kgml")
    retrieveKGML("00260", organism="hsa", destfile=filename.1, method = "internal")
    retrieveKGML("00020", organism="hsa", destfile=filename.2, method = "internal")

    pathway.260 <- parseKGML(filename.1)
    pathway.020 <- parseKGML(filename.2)

    g.reaction.1 <- KEGGpathway2reactionGraph(pathway.260, uniqueReaction = FALSE)
    g.reaction.2 <- KEGGpathway2reactionGraph(pathway.020, uniqueReaction = FALSE)

    checkTrue(!is.null(names(nodeDataDefaults(g.reaction.1))))
    checkTrue(!is.null(names(nodeDataDefaults(g.reaction.2))))
              
    names(nodeDataDefaults(g.reaction.2))   # NULL

    g.m1 <- mergeGraphs(list(g.reaction.1, g.reaction.2))
    g.m2 <- mergeKEGGgraphs(list(g.reaction.1, g.reaction.2))

} # test_mergeKEGGgraphs
#-------------------------------------------------------------------------------
