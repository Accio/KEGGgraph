kgmlNonmetabolicName2MetabolicName <- function(destfile) {
  return(gsub("non-metabolic", "metabolic", destfile))
}

getKGMLurl <- function(pathwayid, organism="hsa") {
  baseurl <- "https://rest.kegg.jp/get/%s%s/kgml"

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

utils::globalVariables("KEGGPATHID2NAME")

kgmlFileName2PathwayName <- function(filename) {
  data("KEGGPATHID2NAME", package="KEGGgraph", envir=environment())
  basename <- sapply(strsplit(filename, "\\."), function(y) y[[1]])
  basename <- gsub("^[a-z][a-z][a-z]","", basename)
  pathname <- unlist(mget(basename, KEGGPATHID2NAME, ifnotfound=as.list(NA)))
  return(pathname)
}

retrieveKGML <- function(pathwayid, organism, destfile, method="auto", ...) {
  #### now KGML does not differ between metabolic and non-metabolic pathways
  ##  kgml <- getCategoryIndepKGMLurl(pathwayid,organism=organism, method=method, ...)
  kgml <- getKGMLurl(pathwayid=pathwayid, organism=organism)
  if(RCurl::url.exists(kgml)) {
    download.file(kgml, destfile=destfile, method=method,...)
  } else {
    message(paste0("KGML cannot be retrieved, cannot open URL '",kgml,"'."))
  }
  return(invisible(kgml))
}

##------------------------------------------------------------##
## may be obslete in the next main release
##------------------------------------------------------------##
getCategoryIndepKGMLurl <- function(pathwayid, organism="hsa", method="auto",...) {
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
