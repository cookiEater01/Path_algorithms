source("dfs.R")
source("izris.R")
source("modifyM.R")

data <- read.table("labyrinth_1.txt", sep=",", header=F)
data <- as.matrix(data)

data <- rename(data)
data


start <- startPosition(data)
start
finish <- finishPosition(data)
finish
screen <- plotLabyrinth(data)




matrikaSosedov(data)

depth.first(data, -2, -3)
