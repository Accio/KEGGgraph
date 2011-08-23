parsePathwayInfo <- function(root) {
  attrs <- xmlAttrs(root)
  ## required: name, org, number
  name <- attrs[["name"]]
  org <- attrs[["org"]]
  number <- attrs[["number"]]
  ## implied: title, image, link
  title <- getNamedElement(attrs, "title")
  image <- getNamedElement(attrs, "image")
  link <- getNamedElement(attrs, "link")

  return(new("KEGGPathwayInfo",
             name=name,
             org=org,
             number=number,
             title=title,
             image=image,
             link=link))
}

parseGraphics <- function(graphics) {
  if(is.null(graphics))
    return(new("KEGGGraphics"))
  attrs <- xmlAttrs(graphics)
  g <- new("KEGGGraphics",
           name=getNamedElement(attrs,"name"),
           x=as.integer(getNamedElement(attrs,"x")),
           y=as.integer(getNamedElement(attrs,"y")),
           type=getNamedElement(attrs,"type"),
           width=as.integer(getNamedElement(attrs, "width")),
           height=as.integer(getNamedElement(attrs,"height")),
           fgcolor=getNamedElement(attrs, "fgcolor"),
           bgcolor=getNamedElement(attrs, "bgcolor")
           )
  return(g)
  
}

parseEntry <- function(entry) {
  attrs <- xmlAttrs(entry)

  ## required: id, name,type
  entryID <- attrs[["id"]]
  name <- unname(unlist(strsplit(attrs["name"]," ")))
  type <- attrs[["type"]]

  ## implied: link, reaction, map
  link <- getNamedElement(attrs,"link")
  reaction <- getNamedElement(attrs, "reaction")
  map <- getNamedElement(attrs, "map")

  ## graphics
  graphics <- xmlChildren(entry)$graphics
  g <- parseGraphics(graphics)
  
  ## types: ortholog, enzyme, gene, group, compound and map
  if(type != "group") {
    newNode <- new("KEGGNode",
                   entryID=entryID,
                   name=name,
                   type=type,
                   link=link,
                   reaction=reaction,
                   map=map,
                   graphics=g)
  } else if(type=="group") {
    children <- xmlChildren(entry)
    children <- children[names(children) == "component"]
    if(length(children)==0) {
      component <- as.character(NA)
    } else {
      component <- sapply(children, function(x) {
        if(xmlName(x) == "component") {
          return(xmlAttrs(x)["id"])
        } else {
          return(as.character(NA))
        }     
      })
    }
    component <- unname(unlist(component))
    newNode <- new("KEGGGroup",
                   component=component,
                   entryID=entryID,
                   name=name,
                   type=type,
                   link=link,
                   reaction=reaction,
                   map=map,
                   graphics=g
                   )
  }
  return(newNode)
}

parseSubType <- function(subtype) {
  attrs <- xmlAttrs(subtype)
  name <- attrs[["name"]]
  value <- attrs[["value"]]
  return(new("KEGGEdgeSubType",name=name, value=value))
}
parseRelation <- function(relation) {
  attrs <- xmlAttrs(relation)

  ## required: entry1, entry2, type
  entry1 <- attrs[["entry1"]]
  entry2 <- attrs[["entry2"]]
  type <- attrs[["type"]]

  subtypeNodes <- xmlChildren(relation)
  subtypes <- sapply(subtypeNodes, parseSubType)
  newEdge <- new("KEGGEdge",
                 entry1ID=entry1,
                 entry2ID=entry2,
                 type=type,
                 subtype=subtypes
                 )                     
  return(newEdge)
}

## NOT TESTED!
parseReaction <- function(reaction) {
  attrs <- xmlAttrs(reaction)

  ## required: name,type
  name <- attrs[["name"]]
  type <- attrs[["type"]]

  children <- xmlChildren(reaction)

  ## more than one substrate/product possible
  childrenNames <- names(children)
  substrateIndices <- grep("^substrate$", childrenNames)
  productIndices <- grep("^product$", childrenNames)
  substrateName <- substrateAltName <- vector("character", length(substrateIndices))
  productName <- productAltName <- vector("character", length(productIndices))  
  
  for (i in seq(along=substrateIndices)) {
    ind <- substrateIndices[i]
    substrate <- children[[ind]]
    substrateName[i] <- xmlAttrs(substrate)[["name"]]
    substrateChildren <- xmlChildren(substrate)
    if (length(substrateChildren)>0) {
      substrateAlt <- substrateChildren$alt
      substrateAltName[i] <- xmlAttrs(substrateAlt)[["name"]]
    } else {
      substrateAlt <- as.character(NA)
      substrateAltName[i] <- as.character(NA)
    }
  }

  for(i in seq(along=productIndices)) {
    ind <- productIndices[i]
    product <- children[[ind]]
    productName[i] <- xmlAttrs(product)[["name"]]
    productChildren <- xmlChildren(product)
    if(length(productChildren)>0) {
      productAlt <- productChildren$alt
      productAltName[i] <- xmlAttrs(productAlt)[["name"]]
    } else {
      productAlt <- as.character(NA)
      productAltName[i] <- as.character(NA)
    }
  }

  new("KEGGReaction",
      name = name,
      type = type,
      substrateName = substrateName,
      substrateAltName = substrateAltName,
      productName = productName,
      productAltName = productAltName)
}

parseKGML <- function(file) {
  doc <- xmlTreeParse(file, getDTD=FALSE)
  r <- xmlRoot(doc)

  ## possible elements: entry, relation and reaction
  childnames <- sapply(xmlChildren(r), xmlName)
  isEntry <- childnames == "entry"
  isRelation <- childnames == "relation"
  isReaction <- childnames == "reaction"

  ## parse them
  kegg.pathwayinfo <- parsePathwayInfo(r)
  kegg.nodes <- sapply(r[isEntry], parseEntry)
  kegg.edges <- sapply(r[isRelation], parseRelation)
  kegg.reactions <- sapply(r[isReaction], parseReaction)
  names(kegg.nodes) <- sapply(kegg.nodes, getEntryID)

  ## build KEGGPathway object
  pathway <- new("KEGGPathway",
                 pathwayInfo = kegg.pathwayinfo,
                 nodes = kegg.nodes,
                 edges = kegg.edges,
                 reactions = kegg.reactions)
  return(pathway)
}

parseKGML2Graph <- function(file, ...) {
  pathway <- parseKGML(file)
  gR <- KEGGpathway2Graph(pathway, ...)
  return(gR)
}

parseKGML2DataFrame <- function(file,...) {
  gR <- parseKGML2Graph(file, ...)
  
  subtype <- sapply(getKEGGedgeData(gR),
                    function(x) sapply(getSubtype(x), getName))
  subtypeLen <- sapply(subtype,length)
  ents <- strsplit(names(edgeData(gR)), "\\|")
  ent1 <- rep(sapply(ents, "[[", 1), subtypeLen)
  ent2 <- rep(sapply(ents, "[[", 2), subtypeLen)
  tbl <- data.frame(from=ent1,
                    to=ent2,
                    subtype=unlist(subtype))
  
  return(tbl)
}

expandKEGGPathway <- function(pathway) {
  nodes.old <- nodes(pathway)
  edges.old <- edges(pathway)

  ## expand nodes and record mapping between old and new EntryID
  ## attention: duplicated new nodes must be removed
  nodes.new <- list(); entryMap <- list()
  for(i in seq(along=nodes.old)) {
    expanded <- expandKEGGNode(nodes.old[[i]])
    newEntryIDs <- sapply(expanded, getEntryID)
    names(expanded) <- newEntryIDs
    
    nodes.new[[i]] <- expanded
    oldEntryID <- getEntryID(nodes.old[[i]])

    entryMap[[i]] <- data.frame(oldEntryID=I(oldEntryID), newEntryID=I(newEntryIDs))
  }
  nodes.new <- unlist(nodes.new); entryMap <- do.call(rbind, entryMap)
  isDuplicatedNode <- duplicated(sapply(nodes.new, getEntryID))
  nodes.new <- nodes.new[!isDuplicatedNode]

  ## expand edges
  edges.new <- list()
  for(i in seq(along=edges.old)) {
    edge.old <- edges.old[[i]]
    entryIDs.old <- getEntryID(edge.old);
    entry1ID.new <- with(entryMap, newEntryID[ oldEntryID == entryIDs.old[1L] ])
    entry2ID.new <- with(entryMap, newEntryID[ oldEntryID == entryIDs.old[2L] ])
##    stopifnot(length(entry1ID.new)>=1 & length(entry2ID.new)>=1) --> not always the case, in KO files there are missing entries
    if(!(length(entry1ID.new)>=1 & length(entry2ID.new)>=1)) {
      warning("Missing entries detected in the KGML file. If it is not a KO file, please check its integrity\n")
      next;
    }
    expand <- expand.grid(entry1ID.new, entry2ID.new)
    edge.new <- list()
    tmp <- edge.old
    for (j in 1:nrow(expand)) {
      entryID(tmp) <- c(as.character(expand[j,1]), as.character(expand[j,2]))
      edge.new[[j]] <- tmp
    }
    edges.new[[i]] <- edge.new
  }
  edges.new <- unlist(edges.new)

  pathway.new <- pathway
  nodes(pathway.new) <- as.list(nodes.new)
  edges(pathway.new) <- as.list(edges.new)

  return(pathway.new)
}

expandKEGGNode <- function(node) {
  names <- getName(node)
  if(length(names) == 1) {
    ## entryID is overwritten by its name, for the sake of compatibility with expandted ones
    entryID(node) <- names
    return(list(node=node))
  } else {
    expanded <- list()
    for(i in seq(along=names)) {
      newNode <- node
      name(newNode) <- names[i]
      entryID(newNode) <- names[i]
      expanded[[i]] <- newNode
    }
    return(expanded)
  }
}

splitKEGGgroup <- function(pathway) {
  pnodes <- nodes(pathway)
  pedges <- edges(pathway)

  if(length(pedges)==0) return(pathway)
  
  types <- sapply(pnodes, getType)
  if(any(types == "group")) {
    isGroup <- names(pnodes)[types == "group"]
    edgeEntry <- sapply(pedges,getEntryID)
    groupAsID <- edgeEntry[1L,] %in% isGroup | edgeEntry[2L,] %in% isGroup

    newly <- list()
    for (e in pedges[groupAsID]) {
      entryIDs <- getEntryID(e)
      node1comps <- getComponent(pnodes[[ entryIDs[1] ]])
      node2comps <- getComponent(pnodes[[ entryIDs[2] ]])
      if(length(node1comps) == 1 && is.na(node1comps)) next;
      if(length(node2comps) == 1 && is.na(node2comps)) next;
      expandmodel <- expand.grid(node1comps, node2comps)
      enews <- list()
      for (j in 1:nrow(expandmodel)) {
        enews[[j]] <- e
        entryID(enews[[j]]) <- c(as.character(expandmodel[j,1L]),as.character(expandmodel[j,2L]))
      }
      newly <- append(newly, enews)
    }

    newEdges <- pedges[!groupAsID]
    newEdges <- append(newEdges, newly)

    edges(pathway) <- newEdges
  }
  return(pathway)
}

KEGGpathway2Graph <- function(pathway, genesOnly=TRUE, expandGenes=TRUE) {
  stopifnot(is(pathway, "KEGGPathway"))

  pathway <- splitKEGGgroup(pathway)
  
  if(expandGenes) {
    pathway <- expandKEGGPathway(pathway)
  }
  
  knodes <- nodes(pathway)
  kedges <- edges(pathway)

  node.entryIDs <- getEntryID(knodes)
  edge.entryIDs <- getEntryID(kedges)

  ## V as nodes, edL as edges
  V <- node.entryIDs
  edL <- vector("list",length=length(V))
  names(edL) <- V

  if(is.null(nrow(edge.entryIDs))) {## no edge found
    for(i in seq(along=edL)) {
      edL[[i]] <- list()
    }
  } else {
    for(i in 1:length(V)) {
      id <- node.entryIDs[i]
      hasRelation <- id == edge.entryIDs[,"Entry1ID"]
      if(!any(hasRelation)) {
        edL[[i]] <- list(edges=NULL)
      } else {
        entry2 <- unname(unique(edge.entryIDs[hasRelation, "Entry2ID"]))
        edL[[i]] <- list(edges=entry2)
      }
    }
  }
  gR <- new("graphNEL", nodes=V, edgeL=edL, edgemode="directed")

  ## set node and edge data - as KEGGNode and KEGGEdge
  ## attention: KEGGEdges may be more than graph edges, due to non-genes
  names(kedges) <- sapply(kedges, function(x) paste(getEntryID(x),collapse="~"))
  
  env.node <- new.env()
  env.edge <- new.env()
  assign("nodes", knodes, envir=env.node)
  assign("edges", kedges, envir=env.edge)
  
  nodeDataDefaults(gR, "KEGGNode") <- env.node
  edgeDataDefaults(gR, "KEGGEdge") <- env.edge

  if(genesOnly) {
    gR <- subGraphByNodeType(gR,"gene")
  }

  return(gR)
}

KEGGpathway2reactionGraph <- function(pathway, uniqueReaction=TRUE) {
  reactions <- getReactions(pathway)
  if(length(reactions)==0) {
    stop("The pathway contains no chemical reactions!\n")
  }

  substrates <- sapply(reactions, getSubstrate)
  products <- sapply(reactions, getProduct)
  nodes <- unique(unlist(c(substrates, products)))

  ## Build adj matrix: since there are one-many, many-one or many-to-many relation between substrate/product
  mat <- matrix(0, nrow=length(nodes), ncol=length(nodes), dimnames=list(nodes, nodes))
  for(i in seq(substrates)) {
    subs <- substrates[[i]]
    pros <- products[[i]]
    if(uniqueReaction) {
      pros <- unique(pros)
    }
    mat[subs, pros] <- mat[subs, pros] + 1
  }

  cg <- as(mat, "graphNEL")

  return(cg)
}

parseKGMLexpandMaps <- function(file, downloadmethod="wget",genesOnly=TRUE, localdir, ...) {
  gR <- parseKGML2Graph(file,expandGenes=TRUE, genesOnly=FALSE)
  
  ismap <- sapply(getKEGGnodeData(gR), getType) == "map"
  mapnames <- sapply(getKEGGnodeData(gR)[ismap], getName)
  mapfiles <- getKGMLurl(mapnames)

  mapfound <- c()
  
  if(!missing(localdir)) {
    localfiles <- dir(localdir, full.name=TRUE)
    mapfound <- match(basename(mapfiles), basename(localfiles))
    needdown <- mapfiles[is.na(mapfound)]
    tmps <- sapply(needdown, function(x) tempfile())
  } else {
    needdown <- mapfiles
    tmps <- sapply(mapfiles, function(x) tempfile())
  }

  for(i in seq(along=needdown)) {
    download.file(needdown[i], tmps[i], method=downloadmethod,...)
  }

  if(!missing(localdir)) {
    tmps <- c(tmps, localfiles[mapfound[!is.na(mapfound)]])
  }
  
  finfos <- sapply(tmps, file.info)
  emptyfiles <- finfos["size",]==0
  if(any(emptyfiles)) {
    warning("The following files are empty!\n", paste(mapfiles[emptyfiles],collapse="\n"))
  }

  mapgrs <- sapply(tmps[!emptyfiles], parseKGML2Graph, genesOnly=genesOnly, expandGenes=TRUE)
  if(genesOnly) {
    gR <- subGraphByNodeType(gR, "gene")
  }
  mapgrs[length(mapgrs)+1] <- gR
  mgr <- mergeGraphs(mapgrs)

  return(mgr)
}
