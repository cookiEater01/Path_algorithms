breadth.first <- function(graph, startNode, endNodes, org)
{
  if (is.null(rownames(graph)))
    vNames <- 1:nrow(graph)
  else
    vNames <- rownames(graph)
  
  startNode <- which(vNames %in% startNode)
  endNodes <- which(vNames %in% endNodes)
  
  #število obdelanih vozlišč
  stVozlisc <- 1
  
  queue <- vector()
  marked <- rep(FALSE, len = nrow(graph))
  from <- rep(-1, len = nrow(graph))
  
  marked[startNode] <- TRUE	
  queue <- c(queue, startNode)
  #print(paste("Dajem v vrsto vozlisce", vNames[startNode]))
  
  
  while (length(queue) > 0)
  {
    curNode <- queue[1]
    queue <- queue[-1]
    stNaPoti<- stNaPoti - 1
    org <- removeCoordB(org, vNames[curNode],ncol(graph))
    plotLabyrinth(org)
    Sys.sleep(0.05)
    #print(paste("Odstranjujem iz vrste vozlisce", vNames[curNode]))
    
    if (curNode %in% endNodes)
    {
      line <- ceiling(strtoi(vNames[curNode]) / sqrt(ncol(graph)))
      coll <- strtoi(vNames[curNode]) - ((line - 1) * sqrt(ncol(graph)))
      #print(paste("Resitev BFS v vozliscu", vNames[curNode]))
      print(paste("Resitev BFS v vozliscu", line, coll))
      print(paste("Stevilo obravnavanih vozlisc:", stVozlisc))
      
      coords <- vNames[curNode]
      #path <- vNames[curNode]
      while (TRUE)
      {
        curNode <- from[curNode]
        if (curNode != -1)
          #path <- paste(path, "<--", vNames[curNode])
          coords <- c(coords, vNames[curNode])
        else
          return(list(org,coords))
      }
    }
    
    for (nextNode in 1:ncol(graph))
    {
      if (!is.na(graph[curNode, nextNode]) && !marked[nextNode])
      {
        marked[nextNode] <- TRUE
        from[nextNode] <- curNode
        queue <- c(queue, nextNode)
        stVozlisc <- stVozlisc + 1
        
        org <- changeCoordB(org, vNames[nextNode], ncol(graph))
        plotLabyrinth(org)
        Sys.sleep(0.05)
        
        #print(paste("Dajem v vrsto vozlisce", vNames[nextNode]))
      }
    }
  }
}