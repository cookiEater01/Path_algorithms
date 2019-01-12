rename <- function(mx) {
  colnames(mx) <- paste("p", 1:ncol(mx), sep="")
  rownames(mx) <- paste("p", 1:nrow(mx), sep="")
  return(mx)
}

startPosition <- function(mx) {
  return(which(mx == -2, TRUE))
}

finishPosition <- function(mx) {
  return(which(mx == -3, TRUE))
}