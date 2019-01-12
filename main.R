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

sosedje <- matrikaSosedov(data)
colnames(sosedje) <- paste("", 1:ncol(sosedje), sep="")
rownames(sosedje) <- paste("", 1:nrow(sosedje), sep="")

startN = row.names(start)
finishN = row.names(finish)

depth.first(sosedje, start, finish)
