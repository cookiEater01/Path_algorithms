depth.first <- function(mtx, startNode, endNodes, org)
{
  if (is.null(rownames(mtx)))
    vNames <- 1:nrow(mtx)
  else
    vNames <- rownames(mtx)
  
  startNode <- which(vNames %in% startNode)
  endNodes <- which(vNames %in% endNodes)
  
  #število obdelanih vozlišč
  stVozlisc <- 1
  #št premikov na poti, brez prvega in zadnjega
  stNaPoti <- -1
  
  stack <- vector()
  marked <- rep(FALSE, len = nrow(mtx))
  from <- rep(-1, len = nrow(mtx))
  
  marked[startNode] <- TRUE	
  stack <- c(stack, startNode)
  #print(paste("Polagam na sklad vozlisce", vNames[startNode])
  
  
  while (length(stack) > 0)
  {
    curNode <- stack[length(stack)]
    
    if (curNode %in% endNodes)
    {
      line <- ceiling(strtoi(vNames[curNode]) / sqrt(ncol(mtx)))
      coll <- strtoi(vNames[curNode]) - ((line - 1) * sqrt(ncol(mtx)))
      print(paste("Resitev DFS v vozliscu", line, coll))
      print(paste("Stevilo obravnavanih vozlisc:", stVozlisc))
      print(paste("Stevilo vozlisc na poti:", stNaPoti))
      
      #path <- vNames[curNode]
      coords <- vNames[curNode]
      while (TRUE)
      {
        curNode <- from[curNode]
        if (curNode != -1) {
          #path <- paste(path, "<--", vNames[curNode])
          coords <- c(coords, vNames[curNode])
        } else
          return(list(org, coords))
      }
    }
    
    sel <- which(!is.na(mtx[curNode,]) & !marked)
    if (any(sel))
    {
      nextNode <- sel[1]
      marked[nextNode] <- TRUE
      from[nextNode] <- curNode
      stack <- c(stack, nextNode)
      stVozlisc <- stVozlisc + 1
      stNaPoti <- stNaPoti + 1
      
      org <- changeCoord(org, vNames[nextNode], stack[length(stack) - 1], ncol(mtx))
      plotLabyrinth(org)
      Sys.sleep(0.1)
      
      
      #print(paste("Polagam na sklad vozlisce", vNames[nextNode]))
    }
    else
    {
      org <- removeCoord(org, stack[length(stack)], stack[length(stack)-1], ncol(mtx))
      plotLabyrinth(org)
      Sys.sleep(0.1)
      stack <- stack[-length(stack)]
      stNaPoti <- stNaPoti - 1
      #print(paste("Odstranjujem s sklada vozlisce", vNames[curNode]))
    }
  }
}