## calculate graph density, defined as the ratio between the number of total edges in a subnetwork and the total number of possible edges
graphDensity <- function(graph) {
  numE <- numEdges(graph)
  numN <- numNodes(graph)
  density <- numE / (numN * (numN-1)/2)
  return(density)
}

mergeGraphs <- function(list, edgemode="directed") {
  ishomo <- isHomoList(list, "graph")
  if(!ishomo)
    stop("'list' must be a list of 'graph' objects!\n")

  ## list must be named, and NO "." is enabled
  nullname <- is.null(names(list))
  if(nullname)
    names(list) <- paste("foo",seq(along=list),sep="")
  names(list)[names(list)==""] <- "foo"
  dupname <- duplicated(names(list))
  if(any(dupname))
    names(list)[dupname] <- paste("foo", seq(along=which(dupname)), sep="")
  names(list) <- gsub("\\.","", names(list))
  
  V <- unique(as.vector(unlist(sapply(list, nodes))))
  E <- unlist(sapply(list, edges, simplify=FALSE), recursive=FALSE, use.names=TRUE)
  names(E) <- sapply(names(E), function(x) unlist(strsplit(x, "\\."))[2])

  uniqueE <- lapply(V, function(v) {
    vedges <- v == names(E)
    uvs <- unique(unlist(E[vedges]))
    return(uvs)
  })
  names(uniqueE) <- V

  g <- new("graphNEL", nodes=V, edgeL=uniqueE, edgemode=edgemode)
  return(g)
}

mergeKEGGgraphs <- function(list, edgemode="directed") {
  ## remove NULL objs
  list <- list[!sapply(list, is.null)]
  
  if(length(list)==0) {
    warning("All items in the input list are NULL")
    return(NULL)
  } else if (length(list)==1) {
    return(list[[1]])
  }
  
  g <- mergeGraphs(list, edgemode=edgemode)


  keggnodes <- unlist(sapply(list, function(x) get("nodes", nodeDataDefaults(x, "KEGGNode"))))
  keggedges <- unlist(sapply(list, function(x) get("edges", edgeDataDefaults(x, "KEGGEdge"))))

  ## merge nodes: use their 'name' attribute as unique index
  keggnodenames <- sapply(keggnodes, getName)
  keggnodeing <- match(nodes(g), keggnodenames)
  mergedkeggnode <- keggnodes[keggnodeing]
  names(mergedkeggnode) <- sapply(mergedkeggnode, getName)

  ## merge edges
  keggedgenames <- sapply(keggedges, getName)
  keggedgeing <- match(getRgraphvizEdgeNames(g), keggedgenames)
  mergedkeggedge <- keggedges[keggedgeing]
  names(mergedkeggedge) <- sapply(mergedkeggedge, getName)

  env.node <- new.env()
  env.edge <- new.env()
  assign("nodes", mergedkeggnode, envir=env.node)
  assign("edges", mergedkeggedge, envir=env.edge)
  
  nodeDataDefaults(g, "KEGGNode") <- env.node
  edgeDataDefaults(g, "KEGGEdge") <- env.edge

  return(g)
}

randomSubGraph <- function(graph, per=0.25, N=10) {
  nodes <- nodes(graph)
  n <- round(length(nodes) * per)

  sapply(1:N, function(x) {
    snodes <- sample(nodes, n)
    subGraph(snodes, graph)
  })
  return(NULL)
}

subKEGGgraph <- function(nodes, graph) {
  subgraph <- subGraph(nodes, graph)
  subnodes <- nodes
  subedges <- getRgraphvizEdgeNames(subgraph)

  keggnodes <- get("nodes",nodeDataDefaults(graph, "KEGGNode"))
  keggedges <- get("edges",edgeDataDefaults(graph, "KEGGEdge"))

  subkeggnodes <- keggnodes[subnodes]
  subkeggedges <- keggedges[subedges]

  env.node <- new.env()
  env.edge <- new.env()
  assign("nodes", subkeggnodes, envir=env.node)
  assign("edges", subkeggedges, envir=env.edge)
  
  nodeDataDefaults(subgraph, "KEGGNode") <- env.node
  edgeDataDefaults(subgraph, "KEGGEdge") <- env.edge

  return(subgraph)
}

subGraphByNodeType <- function(graph, type="gene", kegg=TRUE) {
  kegg.node.data <- getKEGGnodeData(graph)
  
  types <- sapply(kegg.node.data, getType)
  isType <- grep(type,types)
  if(!any(isType)) {
    stop("No '",type, "' type found in the file, maybe it is a map file. Please try parsing the file with 'genesOnly=FALSE'\n")
  }
  
  new.nodes <- names(types[isType])
  if(kegg) {
    sub <- subKEGGgraph(new.nodes, graph)
  } else {
    sub <- subGraph(new.nodes, graph)
  }
  
  return(sub)
}

## getKEGGnode or edge Data
getRgraphvizEdgeNames <- function(graph) {
  edges <- edges(graph)
  sourceNames <- names(edges); edgeNames <- list()
  for (i in seq(along=sourceNames)) {
    if(length(edges[[i]])>0) {
      edgeNames[[i]] <- paste(sourceNames[[i]],edges[[i]],sep="~")
    } else {
      edgeNames[[i]] <- NULL
    }
  }
 edgeNames <- unlist(edgeNames)
 return(edgeNames)
}
getKEGGnodeData <- function(graph, n) {
  knodes <- with(nodeData(graph)[[1]]$KEGGNode, nodes)
  if(missing(n)) {
    return(knodes)
  } else {
    return(knodes[[n]])
  }
}

getKEGGedgeData <- function(graph, n) {
  ed <- edgeData(graph)
  if(length(ed) == 0 ) {## no edge data
    return(list())
  }
  kedges <- with(ed[[1]]$KEGGEdge, edges)
  if(missing(n)) {
    return(kedges)
  } else {
    return(kedges[[n]])
  }
}

neighborhood <- function(graph, index, return.self=FALSE) {
  nds <- nodes(graph)
  mat <- as(graph, "matrix")
  coln <- colnames(mat)
  rown <- rownames(mat)
  index <- as.character(index)
  
  res <- lapply(index, function(x) {
    if(!x %in% nds) return(NULL)

    ed <- coln[mat[x,]==1]
    iEd <- rown[mat[,x]==1]
    y <- c(ed, iEd)

    ## if returns the vertex itself
    if(return.self) y <- c(x,y)
   
    return(unique(y))
  })
  names(res) <- index
  
  return(res)
}

## get subgraph with query GeneIDs
queryKEGGsubgraph <- function(geneids, graph, organism="hsa", addmissing=FALSE) {
  keggids <- translateGeneID2KEGGID(geneids, organism=organism)
  nds <- nodes(graph)

  missing <- !keggids %in% nds
  if(any(missing) & !addmissing) {
    warning('The following GeneIDs can not be found among the nodes of the given graph\n\t', paste(keggids[missing],collapse=", ") )
  }
  missed <- keggids[missing]
  keggids <- unique(keggids[!missing])

  g <- subKEGGgraph(keggids, graph)
  if(addmissing) {
    g <- addNode(missed, g)
  }
  return(g)
}
