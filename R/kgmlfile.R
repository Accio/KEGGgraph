kgmlNonmetabolicName2MetabolicName <- function(destfile) {
  return(gsub("non-metabolic", "metabolic", destfile))
}

getKGMLurl <- function(pathwayid, organism="hsa") {
  baseurl <- "ftp://ftp.genome.jp/pub/kegg/xml/kgml/non-metabolic/organisms"

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

getCategoryIndepKGMLurl <- function(pathwayid, organism="hsa", method="wget",...) {
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
  kgml <- getCategoryIndepKGMLurl(pathwayid,organism=organism, method=method, ...)
  download.file(kgml, destfile=destfile, method=method,...)
  return(invisible(kgml))
}
