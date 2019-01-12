depth.first <- function(mtx, startNode, endNodes)
{
  if (is.null(rownames(mtx)))
    vNames <- 1:nrow(mtx)
  else
    vNames <- rownames(mtx)
  
  startNode <- which(vNames %in% startNode)
  endNodes <- which(vNames %in% endNodes)
  print(endNodes)
  
  #število obdelanih vozlišč
  stVozlisc <- 0
  
  stack <- vector()
  marked <- rep(FALSE, len = nrow(mtx))
  from <- rep(-1, len = nrow(mtx))
  
  marked[startNode] <- TRUE	
  stack <- c(stack, startNode)
  print(paste("Polagam na sklad vozlisce", vNames[startNode]))
  
  
  while (length(stack) > 0)
  {
    curNode <- stack[length(stack)]
    
    if (curNode %in% endNodes)
    {
      print(paste("Resitev DFS v vozliscu", vNames[curNode]))
      print(paste("Stevilo obravnavanih vozlisc:", stVozlisc))
      
      path <- vNames[curNode]
      while (TRUE)
      {
        curNode <- from[curNode]
        if (curNode != -1)
          path <- paste(path, "<--", vNames[curNode])
        else
          return(path)
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
      
      print(paste("Polagam na sklad vozlisce", vNames[nextNode]))
    }
    else
    {
      stack <- stack[-length(stack)]
      print(paste("Odstranjujem s sklada vozlisce", vNames[curNode]))
    }
  }
}