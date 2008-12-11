getKGMLurl <- function(pathwayid, organism="hsa") {
  baseurl <- "ftp://ftp.genome.jp/pub/kegg/xml/organisms"

  pathwayid <- gsub("path:","",pathwayid)
  pco <- grep("^[a-z][a-z][a-z]", pathwayid)
  pco <- pco == seq(along=pathwayid)
  ispco <- length(pco) > 0 & all(pco)
  if(ispco) { ## pathwayid contains organism code
    organism <- sapply(pathwayid, function(x) substr(x, 1,3))
    id <- pathwayid
  } else { ## pathwayid contains no organism code
    id <- paste(organism, pathwayid, sep="")
  }

  idfile <- paste(id,".xml",sep="")
  urls <- paste(baseurl, organism, idfile, sep="/")
  return(urls)
}

kgmlFileName2PathwayName <- function(filename) {
  if(require(KEGG.db)) {
    basename <- sapply(strsplit(filename, "\\."), function(y) y[[1]])
    basename <- gsub("^[a-z][a-z][a-z]","", basename)
    pathname <- unlist(mget(basename, KEGGPATHID2NAME, ifnotfound=as.list(NA)))
    return(pathname)
  } else {
    return(as.character(NA))
  }
}
