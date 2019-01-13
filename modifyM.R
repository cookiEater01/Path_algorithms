rename <- function(mx) {
  #colnames(mx) <- paste("", 1:ncol(mx), sep="")
  #rownames(mx) <- paste("", 1:nrow(mx), sep="")
  colnames(mx) <- c(1:nrow(mx))
  rownames(mx) <- c(1:ncol(mx))
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
      trenutna <-  mx[line, coll] != -1
      enprej <- mx[line, coll - 1] 
      ennaprej <- mx[line, coll + 1]
      engor <- mx[line-1, coll]
      endol <- mx[line+1, coll]
      
      trenPoz <- nrow(mx)*(line-1)+coll
      prejPoz <- nrow(mx)*(line-1)+coll-1
      naprejPoz <- nrow(mx)*(line-1)+coll+1
      gorPoz <- nrow(mx)*(line-2)+coll
      dolPoz <- nrow(mx)*(line)+coll
      
      if ((enprej > 0 || enprej == -3 || enprej == -2) && (trenutna)) {
        ms[trenPoz, prejPoz] <- enprej
      }
      if ((ennaprej > 0 || ennaprej == -3 || ennaprej == -2) && trenutna) {
        ms[trenPoz, naprejPoz] <- ennaprej
      }
      if ((engor > 0 || engor == -3 || engor == -2) && trenutna) {
        ms[trenPoz, gorPoz] <- engor
      }
      if ((endol > 0 || endol == -3 || endol == -2) && trenutna) {
        ms[trenPoz, dolPoz] <- endol
      }
    }
  }
  return(ms)
}


pathMatrixes <- function(org, path) {
  for (rov in 3:nrow(path)-1) {
    org[path[rov,1],path[rov,2]] <- -8
    plotLabyrinth(org)
    Sys.sleep(0.1)
  }
}

setsOfCoords <- function(data) {
  sol <- c()
  for (coor in nrow(data):1) {
    sol <- paste(sol, paste("(", data[coor,1],",", data[coor,2],")", sep=""))
  }
  return(sol)
}

convertCoord <- function(data, size) {
  koordinate <- matrix(ncol=2, nrow=length(data))
  s2 <- sqrt(size)
  for (coor in 1:length(data)) {
    line <- ceiling(strtoi(data[coor]) / s2)
    coll <- strtoi(data[coor]) - ((line - 1) * s2)
    koordinate[coor, 1] <- line
    koordinate[coor, 2] <- coll
  }
  return(koordinate)
}


changeCoord <- function(data, pos, prevPos, size) {
  s2 <- sqrt(size)
  line <- ceiling(strtoi(pos) / s2)
  coll <- strtoi(pos) - ((line - 1) * s2)
  if (data[line, coll] > 0) {
    data[line, coll] <- -6 
  }
  line <- ceiling(strtoi(prevPos) / s2)
  coll <- strtoi(prevPos) - ((line - 1) * s2)
  if (data[line, coll] == -6) {
    data[line, coll] <- -5
  }
  return(data)
}

changeCoordB <- function(data, pos, size) {
  s2 <- sqrt(size)
  line <- ceiling(strtoi(pos) / s2)
  coll <- strtoi(pos) - ((line - 1) * s2)
  if (data[line, coll] > 0) {
    data[line, coll] <- -5 
  }
  return(data)
}

removeCoord <- function(data, pos, prevPos, size, org) {
  s2 <- sqrt(size)
  line <- ceiling(strtoi(pos) / s2)
  coll <- strtoi(pos) - ((line - 1) * s2)
  if (data[line, coll] < -3) {
    data[line, coll] <- org[line, coll]
  }
  line <- ceiling(strtoi(prevPos) / s2)
  coll <- strtoi(prevPos) - ((line - 1) * s2)
  if (data[line, coll] < -3 || data[line, coll] > 0) {
    data[line, coll] <- -6
  }
  return(data)
}

removeCoordB <- function(data, pos, size) {
  s2 <- sqrt(size)
  line <- ceiling(strtoi(pos) / s2)
  coll <- strtoi(pos) - ((line - 1) * s2)
  if (data[line, coll] < -3) {
    data[line, coll] <- -5
  }
  return(data)
}

printPrice <- function(pot, data) {
  cena <- 0
  for (korak in 3:nrow(pot)-1) {
    cena <- cena + data[pot[korak, 1], pot[korak, 2]]
  }
  return(cena)
}