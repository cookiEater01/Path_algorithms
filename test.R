source("dfs.R")
source("izris.R")

graph <- matrix(c(NA, 1, 1, 1,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA, 1, 1,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA, 1, 1, 1,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA, 1, 1,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 1, 1,NA,
                  NA,NA,NA,NA,NA, 1,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 1,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), byrow=TRUE, ncol=13, nrow=13)

colnames(graph) <- c("s","a","b","c","d","e","f","g","h","i","j","k","l")
rownames(graph) <- c("s","a","b","c","d","e","f","g","h","i","j","k","l")


depth.firt <- function(graph, startNode, endNodes)
{
  if (is.null(rownames(graph)))
    vNames <- 1:nrow(graph)
  else
    vNames <- rownames(graph)
  
  startNode <- which(vNames %in% startNode)
  endNodes <- which(vNames %in% endNodes)
  
  
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
    
    sel <- which(!is.na(graph[curNode,]) & !marked)
    if (any(sel))
    {
      nextNode <- sel[1]
      marked[nextNode] <- TRUE
      from[nextNode] <- curNode
      stack <- c(stack, nextNode)
      
      print(paste("Polagam na sklad vozlisce", vNames[nextNode]))
    }
    else
    {
      stack <- stack[-length(stack)]
      print(paste("Odstranjujem s sklada vozlisce", vNames[curNode]))
    }
  }
}



depth.firt(graph, "s", c("g","k","l"))

vNames <- rownames(graph)
a <- which(vNames %in% "s")
