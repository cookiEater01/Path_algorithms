iter.deep <- function(graph, startNode, endNodes)
{
  if (is.null(rownames(graph)))
    vNames <- 1:nrow(graph)
  else
    vNames <- rownames(graph)
  
  startNode <- which(vNames %in% startNode)
  endNodes <- which(vNames %in% endNodes)
  
  
  for (depthLimit in 0:nrow(graph))
  {
    print(paste("Globina iskanja je ", depthLimit))
    
    stack <- vector()
    marked <- rep(FALSE, len = nrow(graph))
    from <- rep(-1, len = nrow(graph))
    
    marked[startNode] <- TRUE	
    stack <- c(stack, startNode)
    print(paste("Polagam na sklad vozlisce", vNames[startNode]))
    
    
    while (length(stack) > 0)
    {
      curNode <- stack[length(stack)]
      
      if (curNode %in% endNodes)
      {
        print(paste("Resitev DFS v vozliscu", vNames[curNode]))
        
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
      
      found <- FALSE
      if (length(stack) <= depthLimit)
      {
        sel <- which(!is.na(graph[curNode,]) & !marked)
        if (any(sel))
        {
          found <- TRUE
          nextNode <- sel[1]
          marked[nextNode] <- TRUE
          from[nextNode] <- curNode
          stack <- c(stack, nextNode)
          
          print(paste("Polagam na sklad vozlisce", vNames[nextNode]))
        }
      }
      
      if (!found)
      {
        stack <- stack[-length(stack)]
        print(paste("Odstranjujem s sklada vozlisce", vNames[curNode]))
      }
    }
    
    print("-------------------------------------------------")
  }
}