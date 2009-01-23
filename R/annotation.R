##------------------------------##
## Annotation
##------------------------------##
translateKEGGID2GeneID <- function(x, organism="hsa") {
  if(organism!="hsa") {
    stop("This function so far supports human gene only.\n To translate KEGGIDs of other species please use online service at http://www.genome.jp/kegg/genes.html")
  }
  gid <- gsub("^[a-z][a-z][a-z]:","", x)
  return(gid)
}

translateGeneID2KEGGID <- function(x, organism="hsa") {
  kid <- paste(organism, ":", x,sep="")
  return(kid)
}

## back compatibility
translateKEGG2GeneID <- function(x, organism="hsa") {
  translateKEGGID2GeneID(x, organism=organism)
}

##------------------------------##
## Translate the graph - not only the nodes but
## also the KEGGedge/KEGGnode data
##------------------------------## 
## Two 'set' methods will be moved into parse.R or methods.R as
## soon as they become generics to KEGGgraph. And they will probabily
## be combined with getKEGGedgeData/getKEGGnodeData
setKEGGedgeData <- function(graph, list) {
  env <- new("environment")
  assign("edges", list, envir=env)
  edgeDataDefaults(graph, "KEGGEdge") <- env
  return(graph)
}

setKEGGnodeData <- function(graph, list) {
  env <- new("environment")
  assign("nodes", list, envir=env)
  nodeDataDefaults(graph, "KEGGNode") <- env
  return(graph)
}

translateKEGGgraph <- function(graph, newNodes) {
  stopifnot(length(nodes(graph)) == length(newNodes))
  oldNodes <- nodes(graph)
  nodes(graph) <- newNodes

  ## node
  knd <- getKEGGnodeData(graph)
  newNames <- c()
  for(i in seq(along=knd)) {
    nN <- newNodes[match( knd[[ i ]]@entryID, oldNodes)]
    knd[[ i ]]@entryID <- nN
    knd[[ i ]]@name <- nN
    newNames[ i ] <- nN
  }
  names(knd) <- newNames
  
  ## edge
  ked <- getKEGGedgeData(graph)
  newNames <- c()
  for(i in seq(along=ked)) {
    ked[[ i ]]@entry1ID <- newNodes[match(ked[[ i ]]@entry1ID, oldNodes)]
    ked[[ i ]]@entry2ID <- newNodes[match(ked[[ i ]]@entry2ID, oldNodes)]
    newNames[i] <- paste(ked[[ i ]]@entry1ID, ked[[ i ]]@entry2ID, sep="~")
  }
  names(ked) <- newNames

  graph <- setKEGGedgeData(graph, ked)
  graph <- setKEGGnodeData(graph, knd)

  return(graph)
}
