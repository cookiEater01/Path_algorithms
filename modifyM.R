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
      if (mx[line, coll - 1] > 0 || mx[line, coll - 1] == -3 || mx[line, coll - 1] == -2) {
        ms[nrow(mx)*(line-1)+(coll-1), nrow(mx)*(line-1)+(coll-2)] = mx[line, coll - 1]
      }
      if (mx[line, coll + 1] > 0 || mx[line, coll + 1] == -3 || mx[line, coll + 1] == -2) {
        ms[nrow(mx)*(line-1)+(coll-1), nrow(mx)*(line-1)+(coll)] = mx[line, coll + 1]
      }
      if (mx[line - 1, coll] > 0 || mx[line - 1, coll] == -3 || mx[line - 1, coll] == -2) {
        ms[nrow(mx)*(line-1)+(coll-1), nrow(mx)*(line-1)+(coll-2)] = mx[line - 1, coll]
      }
      if (mx[line + 1, coll] > 0 || mx[line + 1, coll] == -3 || mx[line + 1, coll] == -2) {
        ms[nrow(mx)*(line-1)+(coll-1), nrow(mx)*(line-1)+(coll)] = mx[line + 1, coll]
      }
    }
  }
}