library(testthat)
library(KEGGgraph)

## Alexander Gulliver Bjornholt Gronning reported that this file cannot be exported as data.frame
## when reactions=TRUE
## It turned out the reason is that KEGGedgeData is not necessarily of the same length as edgeData
## Now it is fixed
kgmlFile <- system.file("extdata/hsa05210.xml", package="KEGGgraph")
kgmlDf <- parseKGML2DataFrame(kgmlFile, reactions=TRUE, genesOnly=FALSE)

## Alexander Gulliver Bjornholt Gronning reported that the following file reported error
##   Error in (function (classes, fdef, mtable)  : 
##     unable to find an inherited method for function ‘getName’ for signature ‘"NULL"’
## when reactions=TRUE
## It was found to be caused by compound nodes (e.g. nodes containing more than one nodes), and
## nodes that are not annotated (e.g. glycans)
## it was fixed in 1.43.3
compoundKgml <- system.file("extdata/hsa00062.xml", package="KEGGgraph")
compoundKgmlDf <- parseKGML2DataFrame(compoundKgml, reactions=TRUE, genesOnly=FALSE)