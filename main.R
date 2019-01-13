###############################################################################
#                               SOURCE                                        #
###############################################################################

source("dfs.R")
source("bfs.R")
source("izris.R")
source("modifyM.R")

###############################################################################
#                        PRIPRAVIMO PODATKE                                   #
###############################################################################


data <- read.table("labyrinth_2.txt", sep=",", header=F)
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
dfs <- depth.first(sosedje, trueStart, trueFinish, data)

f <- convertCoord(dfs, ncol(sosedje))

#izpišemo rešitev
setsOfCoords(f)

# #naredimo novo matriko z potjo
# pathM <- pathMatrix(dfs, ncol(data), nrow(data))
# 
# #zdržimo še podatke splepih poti, starta in finisha
# pathM <- joinMatrixes(data, pathM)
# 
# #izrisemo pot in labirint
# screen <- plotLabyrinth(pathM)


###############################################################################
#                                 BFS                                         #
###############################################################################

#naredimo bfs
bfs <- breadth.first(sosedje, trueStart, trueFinish, data)

g <- convertCoord(bfs, ncol(sosedje))
#izpišemo rešitev
setsOfCoords(g)

# #naredimo novo matriko z potjo
# pathM <- pathMatrix(bfs, ncol(data), nrow(data))
# 
# #zdržimo še podatke splepih poti, starta in finisha
# pathM <- joinMatrixes(data, pathM)
# 
# #izrisemo pot in labirint
# screen <- plotLabyrinth(pathM)
