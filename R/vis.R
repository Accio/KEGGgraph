pvalue2asterisk <- function(pvalues, sig.1 =FALSE) {
  sig0 <- pvalues <= 0.1 | pvalues >= 0.9
  sig1 <- pvalues <= 0.05 | pvalues >= 0.95
  sig2 <- pvalues <= 0.01 | pvalues >= 0.99
  sig3 <- pvalues <= 0.001 | pvalues >= 0.999

  ast <- character(length(pvalues))
  if(sig.1)
    ast[sig0] <- "."
  ast[sig1] <- "*"
  ast[sig2] <- "**"
  ast[sig3] <- "***"

  return(ast)
}

plotKEGGgraph <- function(graph,y="neato",shortLabel=TRUE, nodeRenderInfo,...) {
  nLabel <- getDisplayName(graph,shortLabel=shortLabel)

  subdisplay <- subtypeDisplay(graph)
  eLabel <- subdisplay["label",]
  eCol <- subdisplay["color",]
  eTextCol <- subdisplay["fontcolor",]
  eLty <- subdisplay["style",]
  eArrowhead <- subdisplay["arrowhead",]

  ##  when subdisplay's width is 1
  if (ncol(subdisplay)==1) {
    tmp <- colnames(subdisplay)[1]
    names(eLabel) <- names(eCol) <- names(eTextCol) <- tmp
    names(eLty) <- names(eArrowhead) <- tmp
  }
  
  graph <- layoutGraph(graph, edgeAttrs = list(label=eLabel), nodeAttrs = list(label=nLabel))
  edgeRenderInfo(graph) <- list(lty=eLty, col=eCol, textCol=eTextCol, label=eLabel ,arrowhead=eArrowhead)
  if(!missing(nodeRenderInfo))
    nodeRenderInfo(graph) <- nodeRenderInfo
    
  renderGraph(graph)
  return(graph)
}

KEGGgraphLegend <- function() {
  opar <- par(mar=c(0,0,3,0), mgp=c(0,0,0))
  on.exit(par(opar))
  
  if(!exists("KEGGEdgeSubtype")) {
    data(KEGGEdgeSubtype)
  }

  subtypes <- KEGGEdgeSubtype$name
  cols <- as.character(with(KEGGEdgeSubtype, color))
  labels <- as.character(with(KEGGEdgeSubtype, label))
  fontcolors <- as.character(with(KEGGEdgeSubtype, fontcolor))
  arrowheads <- as.character(with(KEGGEdgeSubtype, arrowhead))
  styles <- as.character(with(KEGGEdgeSubtype, style))
  ltytrans <- c("solid"=1, "dashed"=2, "dotted"=3)
  ltys <- ltytrans[styles]

  plot(1,1, type="n", xlim=c(0.2,2), ylim=c(0,nrow(KEGGEdgeSubtype)+1), axes=FALSE, xlab="", ylab="", main="KEGG diagram legend")
  for(i in 1:nrow(KEGGEdgeSubtype)) {
    text(0.8, i, subtypes[i], pos=2, cex=1.2)
    segments(1,i,2,i, col=cols[i], lty=ltys[i])
    text(1.5, i, labels[i], col=fontcolors[i], pos=2)
    if(arrowheads[i] == "open") {
      text(1.95, i, ">", pos=4, col=cols[i])
    }
  }
}
