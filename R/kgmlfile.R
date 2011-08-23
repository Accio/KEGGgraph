kgmlNonmetabolicName2MetabolicName <- function(destfile) {
  return(gsub("non-metabolic", "metabolic", destfile))
}

getKGMLurl <- function(pathwayid, organism="hsa") {
  ## baseurl <- "ftp://ftp.genome.jp/pub/kegg/xml/kgml/non-metabolic/organisms"
  baseurl <- "http://www.genome.jp/kegg-bin/download?entry=%s%s&format=kgml"

  pathwayid <- gsub("path","",pathwayid)
  pathwayid <- gsub(":","",pathwayid)
  pco <- grepl("^[a-z][a-z][a-z]", pathwayid)
  
  org.len <- length(organism)
  if(org.len==1 & length(pathwayid)!=1) {
    organisms <- rep(organism, length(pathwayid))    
    organisms[pco] <- sapply(pathwayid[pco], function(x) substr(x, 1L, 3L))
  } else if (org.len == length(pathwayid)) {
    organisms <- organism
  } else {
    stop("The length of 'organism' must be either one or the length of 'pathwayid'\n")
  }

  ids <- pathwayid
  ids[pco] <- sapply(pathwayid[pco], function(x) substr(x, 4L, nchar(x)))
  
  urls <- sprintf(baseurl, organisms, ids)
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

retrieveKGML <- function(pathwayid, organism, destfile, method="wget", ...) {
  #### now KGML does not differ between metabolic and non-metabolic pathways
  ##  kgml <- getCategoryIndepKGMLurl(pathwayid,organism=organism, method=method, ...)
  kgml <- getKGMLurl(pathwayid=pathwayid, organism=organism)
  download.file(kgml, destfile=destfile, method=method,...)
  return(invisible(kgml))
}

##------------------------------------------------------------##
## may be obslete in the next main release
##------------------------------------------------------------##
getCategoryIndepKGMLurl <- function(pathwayid, organism="hsa", method="wget",...) {
  .Deprecated(msg="No longer needed, and will be removed in the next main release")
  kgml <- getKGMLurl(pathwayid=pathwayid, organism=organism)
  categoryIndepKGMLurl <- ""
  downloadStatCode <- suppressWarnings(download.file(kgml, destfile=tempfile(),method=method))
  if(downloadStatCode == 0) {
    categoryIndepKGMLurl <- kgml
  } else {
    categoryIndepKGMLurl <- kgmlNonmetabolicName2MetabolicName(kgml)
  }
  return(categoryIndepKGMLurl)
}
