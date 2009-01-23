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
