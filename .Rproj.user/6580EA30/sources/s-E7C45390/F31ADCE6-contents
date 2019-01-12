breadth.first <- function(graph, startNode, endNodes)
{
  if (is.null(rownames(graph)))
    vNames <- 1:nrow(graph)
  else
    vNames <- rownames(graph)
  
  startNode <- which(vNames %in% startNode)
  endNodes <- which(vNames %in% endNodes)
  
  
  queue <- vector()
  marked <- rep(FALSE, len = nrow(graph))
  from <- rep(-1, len = nrow(graph))
  
  marked[startNode] <- TRUE	
  queue <- c(queue, startNode)
  print(paste("Dajem v vrsto vozlisce", vNames[startNode]))
  
  
  while (length(queue) > 0)
  {
    curNode <- queue[1]
    queue <- queue[-1]
    print(paste("Odstranjujem iz vrste vozlisce", vNames[curNode]))
    
    if (curNode %in% endNodes)
    {
      print(paste("Resitev BFS v vozliscu", vNames[curNode]))
      
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
    
    for (nextNode in 1:ncol(graph))
    {
      if (!is.na(graph[curNode, nextNode]) && !marked[nextNode])
      {
        marked[nextNode] <- TRUE
        from[nextNode] <- curNode
        queue <- c(queue, nextNode)
        print(paste("Dajem v vrsto vozlisce", vNames[nextNode]))
      }
    }
  }
}