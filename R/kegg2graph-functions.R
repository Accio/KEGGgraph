##----------------------------------------##
## Class definitions
##----------------------------------------##
setClass("KEGGPathwayInfo",
         representation(name="character",
                        org="character",
                        number="character",
                        title="character",
                        image="character",
                        link="character")
         )

setClass("KEGGPathway",
         representation(pathwayInfo="KEGGPathwayInfo",
                        nodes="list",
                        edges="list",
                        reactions="list")
         )


setClass("KEGGGraphics",
         representation(name="character",
                        x="integer",
                        y="integer",
                        type="character",
                        width="integer",
                        height="integer",
                        fgcolor="character",
                        bgcolor="character"),
         prototype(name=as.character(NA),
                   x=as.integer(NA),
                   y=as.integer(NA),
                   type=as.character(NA),
                   width=as.integer(NA),
                   height=as.integer(NA),
                   fgcolor=as.character(NA),
                   bgcolor=as.character(NA))
         )

setClass("KEGGNode",
         representation(entryID="character",
                        name="character",
                        type="character",
                        link="character",
                        reaction="character",
                        map="character",
                        graphics="KEGGGraphics"
                        )
         )

setClass("KEGGGroup",
         representation(component="character"),
         contains="KEGGNode"
         )

setClass("KEGGEdge",
         representation(entry1ID="character",
                        entry2ID="character",
                        type="character",
                        subtype="list"))

setClass("KEGGEdgeSubType",
         representation(name  = "character",
                        value = "character"),
         prototype(name=as.character(NA),
                   value=as.character(NA)))

setClass("KEGGReaction",
         representation(name = "character",
                        type = "character",
                        substrateName = "character",
                        substrateAltName = "character",
                        productName = "character",
                        productAltName = "character"),
         prototype(name = as.character(NA),
                   type = as.character(NA),
                   substrateName = as.character(NA),
                   substrateAltName=as.character(NA),
                   productName = as.character(NA),
                   productAltName=as.character(NA))              
         )

##----------------------------------------##
## Generics
##----------------------------------------##
setGeneric("getKEGGID", function(object,...) standardGeneric("getKEGGID"))
setGeneric("getEntryID", function(obj) standardGeneric("getEntryID"))
setGeneric("getType", function(object) standardGeneric("getType"))
setGeneric("getDisplayName", function(object, ...) standardGeneric("getDisplayName"))
setGeneric("getName", function(object) standardGeneric("getName"))
setGeneric("getSubtype",function(object) standardGeneric("getSubtype"))
setGeneric("getValue", function(object) standardGeneric("getValue"))
setGeneric("getTitle", function(object) standardGeneric("getTitle"))
setGeneric("getComponent", function(object) standardGeneric("getComponent"))
setGeneric("getPathwayInfo", function(object) standardGeneric("getPathwayInfo"))
setGeneric("getReactions", function(object) standardGeneric("getReactions"))
setGeneric("name<-", function(object, value) standardGeneric("name<-"))
setGeneric("entryID<-", function(object, value) standardGeneric("entryID<-"))
setGeneric("edges<-", function(object,value) standardGeneric("edges<-"))
setGeneric("getKEGGgeneLink", function(object) standardGeneric("getKEGGgeneLink"))
setGeneric("subtypeDisplay", function(object,...) standardGeneric("subtypeDisplay"))
setGeneric("getSubstrate", function(object,...) standardGeneric("getSubstrate"))
setGeneric("getProduct", function(object,...) standardGeneric("getProduct"))

##----------------------------------------##
## Methods
##----------------------------------------##

setMethod("show", "KEGGNode",
          function(object) {
            str <- paste("KEGG Node (Entry '",object@entryID,"'):\n",
                         "------------------------------------------------------------\n",
                         "[ displayName ]: ", getDisplayName(object),"\n",
                         "[ Name ]: ", paste(object@name, collapse=","),"\n",
                         "[ Type ]: ", object@type, "\n",
                         "[ Link ]: ", object@link, "\n",
                         "------------------------------------------------------------\n",sep="")
            cat(str)                         
          })

setMethod("show","KEGGEdgeSubType",function(object) {
  str <- paste("  [ Subtype name ]: ", object@name,"\n",
               "  [ Subtype value ]: ", object@value,"\n",sep="")
  cat(str)
})

setMethod("show", "KEGGEdge",
          function(object) {

            str <- paste("  KEGG Edge (Type: ", object@type,"):\n",
                         "------------------------------------------------------------\n",
                         "[ Entry 1 ID ]: ", object@entry1ID, "\n",
                         "[ Entry 2 ID ]: ", object@entry2ID, "\n",
                         "[ Subtype ]: \n",sep="")
            cat(str)
            sapply(object@subtype, show)
            cat("------------------------------------------------------------\n")
          })
setMethod("show","KEGGPathway",
          function(object) {
            cat("KEGG Pathway\n")
            show(getPathwayInfo(object))
            str <- paste("------------------------------------------------------------\n",
                         "Statistics:\n",
                         "\t", length(nodes(object)), " node(s)\n",
                         "\t", length(edges(object)), " edge(s)\n",
                         "\t", length(getReactions(object)), " reaction(s)\n",
                          "------------------------------------------------------------\n",sep=""
                         )
            cat(str)
          })
setMethod("show","KEGGPathwayInfo",
          function(object) {
            str <- paste("[ Title ]: ", object@title,"\n",
                         "[ Name ]: ", object@name, "\n",
                         "[ Organism ]: ", object@org, "\n",
                         "[ Number ] :", object@number, "\n",
                         "[ Image ] :", object@image, "\n",
                         "[ Link ] :", object@link, "\n",sep="")
            cat(str)
          })
setMethod("show", "KEGGReaction",
          function(object) {
            salt <- object@substrateAltName; palt <- object@productAltName
            saltstr <- ""; paltstr <- ""
            if(!is.na(salt)) saltstr <- paste("\t[ Substrate Alternative Name ]: ", salt, "\n", sep="")
            if(!is.na(palt)) paltstr <- paste("\t[ Substrate Alternative Name ]: ", palt, "\n", sep="")
            
            str <- paste("KEGG Reaction(", object@name, ")\n",
                         "------------------------------------------------------------\n",
                         "[ Name ]: ", object@name, "\n",
                         "[ Type ]: ", object@type, "\n",
                         "[ Substrate Name ]: ",object@substrateName , "\n",
                         saltstr,
                         "[ Product Name ]: ", object@productName,"\n",
                         paltstr,sep="")
            cat(str)
          })
          
.getEntryID <- function(x) x@entryID
.getType <- function(x) x@type
.getLink <- function(x) x@link
.getTitle <- function(x) x@title
.getGraphics <- function(x) x@graphics
.getEntry1ID <- function(x) x@entry1ID
.getEntry2ID <- function(x) x@entry2ID
.getName <- function(x) x@name
.getSubtype <- function(x) x@subtype
.getValue <- function(x) x@value
.getTitle <- function(x) x@title
.getComponent <- function(x) x@component

setMethod("nodes","KEGGPathway", function(object) {
  return(object@nodes)
})
setMethod("edges", "KEGGPathway", function(object, which) {
  return(object@edges)
})
setMethod("getReactions", "KEGGPathway", function(object) {
  return(object@reactions)
})
setMethod("getPathwayInfo", "KEGGPathway", function(object) {
  return(object@pathwayInfo)
})

setMethod("getKEGGID", "KEGGNode", function(object, removePrefix=TRUE) {
  names <- .getName(object)
  if(removePrefix)
    names <- sapply(strsplit(names, ":"), "[[",2)
  return(names)
})
setMethod("getEntryID", "KEGGNode", function(obj) .getEntryID(obj))
setMethod("getName", "KEGGNode", function(object) .getName(object))
setMethod("getName", "KEGGEdge", function(object) {
  name1 <- .getEntry1ID(object)
  name2 <- .getEntry2ID(object)
  return(paste(name1, name2, sep="~"))
})
setMethod("getName","KEGGReaction", function(object) {
  .getName(object)
})
setMethod("getEntryID", "KEGGEdge", function(obj) {
  entry1ID <- .getEntry1ID(obj)
  entry2ID <- .getEntry2ID(obj)
  return(c(Entry1ID=entry1ID, Entry2ID=entry2ID))
})
setMethod("getDisplayName", "KEGGNode", function(object, short=FALSE) {
  g <- .getGraphics(object)
  name <- .getName(g)
  if(short) {
    name <- gsub("\\.\\.\\.","",name)
    name <- unlist(strsplit(name,","))[[1]]
  }
  return(name)
})
setMethod("getDisplayName", "graph", function(object, shortLabel=TRUE) {
  nd <- getKEGGnodeData(object)
  labels <- sapply(nd, function(x) getDisplayName(x, short=shortLabel))
  browser()
  ## labels are all the nodes, object however can be subGraph
  no <- nodes(object)
  isMapped <- no %in% names(labels)
  labels <- labels[no[isMapped]]
  notmapped <- no[!isMapped]; names(notmapped) <- notmapped
  labels <- c(labels, notmapped)
  return(labels)
})

setMethod("getTitle","KEGGPathwayInfo", function(object) .getTitle(object))
setMethod("getTitle","KEGGPathway", function(object) {
  pi <- getPathwayInfo(object)
  return(.getTitle(pi))
})
setMethod("getComponent", "KEGGNode", function(object) getEntryID(object))
setMethod("getComponent", "KEGGGroup", function(object) .getComponent(object))
setMethod("getEntryID", "list", function(obj) {
  if(length(obj) == 0) return(NA)
  isNodeList <- isHomoList(obj, "KEGGNode")
  isEdgeList <- isHomoList(obj, "KEGGEdge")
  stopifnot(isNodeList | isEdgeList)

  ids <- unname(sapply(obj, getEntryID))
  if(isEdgeList) {
    ids <- t(ids)
    colnames(ids) <- c("Entry1ID","Entry2ID")
  }
  return(ids)
})
setMethod("getType", "KEGGEdge", function(object) .getType(object))
setMethod("getType", "KEGGNode", function(object) .getType(object))
setMethod("getType", "KEGGReaction", function(object) .getType(object))
setMethod("getName","KEGGPathwayInfo", function(object) .getName(object))
setMethod("getName", "KEGGPathway", function(object) {
  pi <- getPathwayInfo(object)
  return(getName(pi))
})
setMethod("getSubtype", "KEGGEdge", function(object) .getSubtype(object))
setMethod("getSubtype", "graph", function(object) {
  edgeNames <- getRgraphvizEdgeNames(object)
  edgeData <- sapply(edgeNames, function(x) getKEGGedgeData(object,x))

  subtypes <- lapply(edgeData, getSubtype)
  return(subtypes)
})
setMethod("getName","KEGGEdgeSubType", function(object) .getName(object))
setMethod("getValue", "KEGGEdgeSubType", function(object) .getValue(object))


setReplaceMethod("name","KEGGNode", function(object,value) {
  object@name <- value
  return(object)
})

setReplaceMethod("entryID", "KEGGNode", function(object, value) {
  object@entryID <- value
  return(object)
})
setReplaceMethod("entryID", "KEGGEdge", function(object, value) {
  stopifnot( length(value)==2L )
  object@entry1ID <- value[ 1L ]
  object@entry2ID <- value[ 2L ]
  return(object)
})

setReplaceMethod("nodes", "KEGGPathway", function(object, value) {
  object@nodes <- value
  return(object)
})

setReplaceMethod("edges", "KEGGPathway", function(object, value) {
  object@edges <- value
  return(object)
})


setMethod("getKEGGgeneLink", "character", function(object) {
  if(length(object) == 1)
    suffix <- gsub(":","+",object)
  url <- paste("http://www.genome.jp/dbget-bin/www_bget?",suffix,sep="")
  return(url)
       })


.subtypeDisplay <- function(subtype) {
  if(!exists("KEGGEdgeSubtype")) {
    data(KEGGEdgeSubtype)
  }
  name <- getName(subtype)
  value <- getValue(subtype)

  i <- match(name, KEGGEdgeSubtype$name)

  if(is.na(i)) {
    stop("Given subtype '", name, "' is not found!\n")
  }
  
  row <- KEGGEdgeSubtype[i,]

  
  color <- as.character(row[1L ,"color"])
  label <-  as.character(row[1L ,"label"])
  fontcolor <- as.character(row[1L ,"fontcolor"])
  arrowhead <- as.character(row[1L ,"arrowhead"])
  style <- as.character(row[1L ,"style"])

  disSub <- c(name=name, value=value,
                color=color, label=label,
                fontcolor=fontcolor, arrowhead=arrowhead,style=style)

  return(disSub)
}
setMethod("subtypeDisplay","KEGGEdgeSubType", function(object, attr) {
  obj <- .subtypeDisplay(object)
  return(obj)
})
setMethod("subtypeDisplay", "KEGGEdge",function(object) {
  subtypes <- getSubtype(object)
  if(length(subtypes) == 1) {
    display <- subtypeDisplay(subtypes[[1]])
  } else {
    subtypeNames <- sapply(subtypes, getName)
    subtypeValues <- sapply(subtypes, getValue)
    display <- subtypeDisplay(subtypes[[1]])
    display[["name"]] <- paste(subtypeNames, collapse=",")
    display[["value"]] <- paste(subtypeValues, collapse=",")
  }
  return(display)
})
setMethod("subtypeDisplay", "graph", function(object, attr) {
  keggedges <- getKEGGedgeData(object)
  keggedgesDisplay <- sapply(keggedges, subtypeDisplay)
  if(missing(attr)) return(keggedgesDisplay)
  if(!attr %in% rownames(test)) {
    stop("'attr' must be one of:", paste(rownames(keggeEdgesDisplay), collapse=","),"\n")
  } else {
    return(keggedgesDisplay[attr,])
  }
})

setMethod("getSubstrate", "KEGGReaction", function(object) object@substrateName)
setMethod("getProduct", "KEGGReaction", function(object) object@productName)
