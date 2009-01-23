getNamedElement <- function(vector, name) {
  if (name %in% names(vector))
    return(vector[[name]])
  else
    return(as.character(NA))
}

isHomoList <- function(list, class) {
  if(length(list) == 0 || !is.list(list)) return(FALSE)

  isHomo <- sapply(list, is, class)

  if(all(isHomo)) return(TRUE)
  return(FALSE)
}
