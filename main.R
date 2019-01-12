source("dfs.R")
source("izris.R")
source("modifyM.R")

data <- read.table("labyrinth_2.txt", sep=",", header=F)
data <- as.matrix(data)

data <- rename(data)
data


start <- startPosition(data)
start
finish <- finishPosition(data)
finish
screen <- plotLabyrinth(data)




















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


#graph
screen1 <- plotLabyrinth(graph)


depth.first(graph, "s", c("g","k","l"))

depth.first(data, a, )