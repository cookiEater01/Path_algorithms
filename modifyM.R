rename <- function(mx) {
  colnames(mx) <- paste("", 1:ncol(mx), sep="")
  rownames(mx) <- paste("", 1:nrow(mx), sep="")
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
      trenutna = mx[line, coll] != -1
      enprej = mx[line, coll - 1] 
      ennaprej = mx[line, coll + 1]
      engor = mx[line-1, coll]
      endol = mx[line+1, coll]
      
      trenPoz = nrow(mx)*(line-1)+coll
      prejPoz = nrow(mx)*(line-1)+coll-1
      naprejPoz = nrow(mx)*(line-1)+coll+1
      gorPoz = nrow(mx)*(line-2)+coll
      dolPoz = nrow(mx)*(line)+coll
      
      if ((enprej > 0 || enprej == -3 || enprej == -2) && (trenutna)) {
        ms[trenPoz, prejPoz] = enprej
      }
      if ((ennaprej > 0 || ennaprej == -3 || ennaprej == -2) && trenutna) {
        ms[trenPoz, naprejPoz] = ennaprej
      }
      if ((engor > 0 || engor == -3 || engor == -2) && trenutna) {
        ms[trenPoz, gorPoz] = engor
      }
      if ((endol > 0 || endol == -3 || endol == -2) && trenutna) {
        ms[trenPoz, dolPoz] = endol
      }
    }
  }
  return(ms)
}