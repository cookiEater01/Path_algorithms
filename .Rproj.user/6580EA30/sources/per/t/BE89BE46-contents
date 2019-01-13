iter.deep <- function(graph, startNode, endNodes, org)
{
  if (is.null(rownames(graph)))
    vNames <- 1:nrow(graph)
  else
    vNames <- rownames(graph)

  backup <- org
  
  startNode <- which(vNames %in% startNode)
  endNodes <- which(vNames %in% endNodes)
  
  
  for (depthLimit in 0:nrow(graph))
  {
    #org <- backup
    #print(paste("Globina iskanja je ", depthLimit))
    
    stack <- vector()
    marked <- rep(FALSE, len = nrow(graph))
    from <- rep(-1, len = nrow(graph))
    
    marked[startNode] <- TRUE	
    stack <- c(stack, startNode)
    #print(paste("Polagam na sklad vozlisce", vNames[startNode]))
    
    
    while (length(stack) > 0)
    {
      curNode <- stack[length(stack)]
      
      if (curNode %in% endNodes)
      {
        plotLabyrinth(org)
        line <- ceiling(strtoi(vNames[curNode]) / sqrt(ncol(graph)))
        coll <- strtoi(vNames[curNode]) - ((line - 1) * sqrt(ncol(graph)))
        #print(paste("Resitev BFS v vozliscu", vNames[curNode]))
        print(paste("Resitev DFS v vozliscu", line, coll))
        #print(paste("Resitev DFS v vozliscu", vNames[curNode]))
        print(paste("Globina iskanja je ", depthLimit))
        
        #path <- vNames[curNode]
        coords <- vNames[curNode]
        while (TRUE)
        {
          curNode <- from[curNode]
          if (curNode != -1)
            coords <- c(coords, vNames[curNode])
          else
            return(list(org,coords))
            #path <- paste(path, "<--", vNames[curNode])
          #else
            #return(path)
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

          org <- changeCoordB(org, vNames[nextNode], ncol(graph))
          #plotLabyrinth(org)
          #Sys.sleep(0.1)
          
          #print(paste("Polagam na sklad vozlisce", vNames[nextNode]))
        }
      }
      
      if (!found)
      {
        org <- removeCoordB(org, stack[length(stack)], ncol(graph))
        stack <- stack[-length(stack)]
        #print(paste("Odstranjujem s sklada vozlisce", vNames[curNode]))
      }
    }
    #print("-------------------------------------------------")
  }
}