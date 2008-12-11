library(Rgraphviz)
library(KEGGgraph)
library(RBGL)

## basic parsing
sfile <- system.file("extdata/hsa04010.xml",package="KEGGgraph")

gR <- parseKGML2Graph(sfile,expandGenes=TRUE)
stopifnot(numNodes(gR) == length(getKEGGnodeData(gR)))
stopifnot(numEdges(gR) == length(getKEGGedgeData(gR)))
show(gR)

## alternative parsing
kegg.pathway <- parseKGML(sfile)
show(kegg.pathway)
kegg.nodes <- getKEGGnodeData(gR)
show(kegg.nodes)
kegg.edges <- getKEGGedgeData(gR)
show(kegg.edges)

## splitGroup
kegg.pathway.split <- splitKEGGgroup(kegg.pathway)
gRsplit <- KEGGpathway2Graph(kegg.pathway.split, expandGenes=FALSE)
gRsplitgeneexpand <- KEGGpathway2Graph(kegg.pathway.split)
gRsgenoout <- sapply(edges(gRsplitgeneexpand), function(x) length(x)==0)
gRsgenoin <- sapply(inEdges(gRsplitgeneexpand), function(x) length(x)==0)
gRssingle <- gRsgenoout & gRsgenoin

## compact/non-compact parsing
gR.compact<- KEGGpathway2Graph(kegg.pathway,expandGenes=FALSE)
gR.expanded <- KEGGpathway2Graph(kegg.pathway)

## random subgraph
set.seed(123)
gR.randsub <- subGraph(sample(nodes(gR),10), gR)

## mapk14 == "hsa:1432"
## visualization
subs <-  c("hsa:1432",edges(gR)$`hsa:1432`,"hsa:5778","hsa:5801","hsa:84867","hsa:11072","hsa:5606","hsa:5608","hsa:5494","hsa:5609")
gR.sub <- subGraph(subs, gR)
## wrapped method
##plotKEGGgraph(gR)
plotKEGGgraph(gR.sub)
## not wrapper method
plot(gR.sub, "neato")
plot(gR.compact, "twopi")

## KO pathways
kofile <- system.file("extdata/ko00051.xml",package="KEGGgraph")
kotest <- parseKGML2Graph(kofile, genesOnly=FALSE)

## MAP files
mapfile <-  system.file("extdata/map00260.xml",package="KEGGgraph")
maptest <- parseKGML2Graph(mapfile, genesOnly=FALSE)

## merge graphs
wntfile <- system.file("extdata/hsa04310.xml",package="KEGGgraph")
wntR <- parseKGML2Graph(wntfile, expandGenes=TRUE)
graphlist <- list(mapkG=gR, wntG=wntR)
merged <- mergeGraphs(graphlist)

## subKEGGgraph: similar with subGraph, with KEGGnodeData and KEGGedgeData also subsetted
gR.keggsub <- subKEGGgraph(subs, gR)
stopifnot(length(getKEGGnodeData(gR.keggsub)) == numNodes(gR.keggsub))
stopifnot(length(getKEGGedgeData(gR.keggsub)) == numEdges(gR.keggsub))
plotKEGGgraph(gR.keggsub)

## mergeKEGGgraphs: similar with mergeGraph, with KEGGnodeData and KEGGedgeData also merged
mergedKEGG <- mergeKEGGgraphs(graphlist)
mergedKEGG
stopifnot(length(getKEGGnodeData(mergedKEGG)) == numNodes(mergedKEGG))
stopifnot(length(getKEGGedgeData(mergedKEGG)) == numEdges(mergedKEGG))
