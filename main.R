source("dfs.R")
source("izris.R")
source("modifyM.R")

data <- read.table("labyrinth_1.txt", sep=",", header=F)
data <- as.matrix(data)

data <- rename(data)

start <- startPosition(data)
trueStart = toString(nrow(data) * (start[1,1] - 1) + start[1,2])
finish <- finishPosition(data)
trueFinish = toString(nrow(data) * (finish[1,1] - 1) + finish[1,2])

screen <- plotLabyrinth(data)

sosedje <- matrikaSosedov(data)

colnames(sosedje) <- c(1:nrow(sosedje))
rownames(sosedje) <- c(1:ncol(sosedje))

depth.first(sosedje, trueStart, trueFinish)
