# test_KEGGgraph.R
#-------------------------------------------------------------------------------
# move these to paulsTests when development slows
library(KEGGgraph)
library(RUnit)
#-------------------------------------------------------------------------------
# this test verifies the successful remedy of the difficulty encountered by
# Stuart Bradley, in which mergeGraphs works with two reaction graphs, but
@ mergeKEGGgraphs # does not.
# the underlying problem appears to be  mergeKEGGgraphs (in KEGGgraph/R/graph.R)
# requires that there be an edge attribute "KEGGEdge" and a node attribute
# "KEGGNode", and fails when those are not found
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

    g.reaction.1 <- KEGGpathway2reactionGraph(pathway.260)
    g.reaction.2 <- KEGGpathway2reactionGraph(pathway.020)

    checkTrue(!is.null(names(nodeDataDefaults(g.reaction.1))))
    checkTrue(!is.null(names(nodeDataDefaults(g.reaction.2))))
              
    checkTrue("KEGGNode" %in% names(nodeDataDefaults(g.reaction.1)))
    checkTrue("KEGGEdge" %in% names(edgeDataDefaults(g.reaction.1)))
    checkTrue("KEGGNode" %in% names(nodeDataDefaults(g.reaction.2)))
    checkTrue("KEGGEdge" %in% names(edgeDataDefaults(g.reaction.2)))

    g.m1 <- mergeGraphs(list(g.reaction.1, g.reaction.2))
    g.m2 <- mergeKEGGgraphs(list(g.reaction.1, g.reaction.2))
    checkEquals(sort(nodes(g.m1)), sort(nodes(g.m2)))

} # test_mergeKEGGgraphs
#-------------------------------------------------------------------------------
# hoping to get a data.frame with all nodes (genes, compounds, pathways,
# metabolites) and all reactions, relations and # interactions, from membership
# in compounds, to catalysis, substrate and product relations, to activations:
# the full kegg # pathway without anything left out
# maybe this is all provided with the new version, but right now the function
# fails with the error noted below.
#
test_parseKGML2DataFrame <- function()
{
    print("test_parseKGML2DataFrame")

    dir <- tempdir()
    filename <- file.path(dir, "hsa00020.kgml")
    retrieveKGML("00020", organism="hsa", destfile=filename.2, method = "internal")
    pathway <- parseKGML(filename)
    #tbl <- parseKGML2DataFrame(filename, genesOnly=FALSE)
       # produces this error:
       # Error in rep(sapply(ents, "[[", 1), subtypeLen) : 
       #  invalid 'times' argument

          
} # test_parseKGML2DataFrame
#-------------------------------------------------------------------------------
