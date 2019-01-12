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

matrikaSosedov <- function(mx) {
  vel = nrow(mx) * ncol(mx)
  ms <- matrix(data=NA, nrow=vel, ncol=vel)
  for (line in c(2:nrow(mx))) {
    for (coll in c(2:ncol(mx))) {
       if (mx[line, coll - 1] > 0) {
         
       }
    }
  }
}