a.star <- function(graph, startNode, endNodes)
{
  if (is.null(rownames(graph)))
    vNames <- 1:nrow(graph)
  else
    vNames <- rownames(graph)
  
  startNode <- which(vNames %in% startNode)
  endNodes <- which(vNames %in% endNodes)
  
  
  open <- rep(FALSE, len = nrow(graph))
  closed <- rep(FALSE, len = nrow(graph))
  
  gScore <- rep(Inf, len = nrow(graph))
  gScore[startNode] <- 0
  
  fScore <- rep(Inf, len = nrow(graph))
  
  from <- rep(-1, len = nrow(graph))
  
  open[startNode] <- TRUE
  print(paste("Odpiram vozlisce", vNames[startNode]))
  
  while (any(open))
  {
    sel <- which.min(fScore[open])
    curNode <- which(open)[sel]
    
    open[curNode] <- FALSE
    closed[curNode] <- TRUE
    print(paste("Zapiram vozlisce", vNames[curNode]))
    
    if (curNode %in% endNodes)
    {
      print(paste("Resitev A* v vozliscu", vNames[curNode]))
      
      #path <- vNames[curNode]
      coord <- vNames[curNode]
      while (TRUE)
      {
        curNode <- from[curNode]
        if (curNode != -1)
          #path <- paste(path, "<--", vNames[curNode])
          coord <- c(coord, vNames[curNode])
        else
          return(coord)
      }
    }
    
    for (nextNode in 1:ncol(graph))
    {
      if (!is.na(graph[curNode, nextNode]) && !closed[nextNode])
      {
        if (!open[nextNode])
          print(paste("Odpiram vozlisce", vNames[nextNode])) 
        
        open[nextNode] <- TRUE
        dist <- gScore[curNode] + graph[curNode, nextNode]
        
        if (dist < gScore[nextNode])
        {
          from[nextNode] <- curNode
          gScore[nextNode] <- dist
          fScore[nextNode] <- gScore[nextNode]
          print(paste("Posodabljam vozlisce", vNames[nextNode]))
        }
      }
    }
  }
}
