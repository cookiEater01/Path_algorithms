###############################################################################
#                               SOURCE                                        #
###############################################################################

source("dfs.R")
source("bfs.R")
source("iddfs.R")
source("izris.R")
source("modifyM.R")
#install.packages("gsubfn")
library("gsubfn")

###############################################################################
#                        PRIPRAVIMO PODATKE                                   #
###############################################################################


data <- read.table("labyrinth_11.txt", sep=",", header=F)
data <- as.matrix(data)

#preimenujemo zaradi lažjega branja
data <- rename(data)

#določimo stolpec in vrstico starta
start <- startPosition(data)

#določimo številko štartnega kvadratka
trueStart = toString(nrow(data) * (start[1,1] - 1) + start[1,2])

#ponovimo še za finish
finish <- finishPosition(data)

#le da naredimo vektor, saj je ciljev več
trueFinish = c()
for (var in 1:nrow(finish)) {
  trueFinish <- c(trueFinish, toString(nrow(data) * (finish[var,1] - 1) + finish[var,2]))
}

screen <- plotLabyrinth(data)

#naredimo matriko sosedov
sosedje <- matrikaSosedov(data)

#preimenujemo stolpce in vrstice v številke kvadratkov
#colnames(sosedje) <- c(1:nrow(sosedje))
#rownames(sosedje) <- c(1:ncol(sosedje))
sosedje <- rename(sosedje)


###############################################################################
#                                 DFS                                         #
###############################################################################


#naredimo dfs
list[pot,dfs] <- depth.first(sosedje, trueStart, trueFinish, data)

f <- convertCoord(dfs, ncol(sosedje))

print(paste("Stevilo vozlisc na poti:", nrow(f)))

printPrice(f, data)

pathMatrixes(pot, f)
#izpišemo rešitev
setsOfCoords(f)



###############################################################################
#                                 BFS                                         #
###############################################################################

#naredimo bfs
list[pot,bfs] <- breadth.first(sosedje, trueStart, trueFinish, data)

g <- convertCoord(bfs, ncol(sosedje))

print(paste("Stevilo vozlisc na poti:", nrow(g)))

printPrice(g, data)

pathMatrixes(pot,g)
#plotLabyrinth(pathMatrixes(pot,g))

#izpišemo rešitev
setsOfCoords(g)

###############################################################################
#                                 IDFS                                        #
###############################################################################

#izvedemo iterativni deep search
list[pot, idfs] <- iter.deep(sosedje, trueStart, trueFinish, data)

i <- convertCoord(idfs, ncol(sosedje))

print(paste("Stevilo obravnavanih ogljisc: ", nrow(which(pot == -5, T))))
print(paste("Stevilo vozlisc na poti:", nrow(i)))

printPrice(i, data)

pathMatrixes(pot,i)

#izpišemo rešitev
setsOfCoords(i)