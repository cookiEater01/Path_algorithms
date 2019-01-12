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
  for (line in c(3:nrow(mx)-1)) {
    for (coll in c(3:ncol(mx)-1)) {
      trenutna = ms[line, coll] != -1
      if ((mx[line, coll - 1] > 0 || mx[line, coll - 1] == -3 || mx[line, coll - 1] == -2) && (trenutna)) {
        ms[nrow(mx)*(line-1)+(coll-1), nrow(mx)*(line-1)+(coll-2)] = mx[line, coll - 1]
      }
      if ((mx[line, coll + 1] > 0 || mx[line, coll + 1] == -3 || mx[line, coll + 1] == -2) && trenutna) {
        ms[nrow(mx)*(line-1)+(coll-1), nrow(mx)*(line-1)+(coll)] = mx[line, coll + 1]
      }
      if ((mx[line - 1, coll] > 0 || mx[line - 1, coll] == -3 || mx[line - 1, coll] == -2) && trenutna) {
        ms[nrow(mx)*(line-1)+(coll-1), nrow(mx)*(line-2)+(coll-1)] = mx[line - 1, coll]
      }
      if ((mx[line + 1, coll] > 0 || mx[line + 1, coll] == -3 || mx[line + 1, coll] == -2) && trenutna) {
        ms[nrow(mx)*(line-1)+(coll-1), nrow(mx)*(line)+(coll-1)] = mx[line + 1, coll]
      }
    }
  }
  return(ms)
}
