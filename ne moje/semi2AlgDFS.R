library(grid)
plotLabyrinth <- function(lab)
{
	sel <- lab[,] == -1
	lab[sel] <- rgb(0, 0, 0)		
	
	sel <- lab[,] == 0
	lab[sel] <- rgb(1, 1, 1)

	sel <- lab[,] == -2
	lab[sel] <- rgb(1, 0, 0)

	sel <- lab[,] == -3
	lab[sel] <- rgb(1, 1, 0)
	
	sel <- lab[,] == -4
	lab[sel] <- rgb(0, 0, 1)

	sel <- lab[,] == 1
	lab[sel] <- rgb(0.3, 0.3, 0.3)

	sel <- lab[,] == 2
	lab[sel] <- rgb(0.4, 0.4, 0.4)

	sel <- lab[,] == 3
	lab[sel] <- rgb(0.6, 0.6, 0.6)

	sel <- lab[,] == 4
	lab[sel] <- rgb(0.8, 0.8, 0.8)

	grid.newpage()
	grid.raster(lab, interpolate=F)
}


matrikaSosedov <- function(data){
	Sys.sleep(0.1)
	currentPosition=which(data == -2,TRUE)
	data[currentPosition[1],currentPosition[2]]=-4	

	dol=data[currentPosition[1]+1,currentPosition[2]]
	desno=data[currentPosition[1],currentPosition[2]+1]
	gor=data[currentPosition[1]-1,currentPosition[2]]
	levo=data[currentPosition[1],currentPosition[2]-1]
	
	pathFound=FALSE
	if (dol == -3) return(TRUE)
	if (!pathFound && dol > 0){
		tempInt=data[currentPosition[1]+1,currentPosition[2]]
		data[currentPosition[1]+1,currentPosition[2]] = -2
		screen <- plotLabyrinth(data)
		pathFound=matrikaSosedov(data)
		data[currentPosition[1]+1,currentPosition[2]] = tempInt
	}
	if (desno == -3) return(TRUE)
	if (!pathFound && desno > 0){
		tempInt=data[currentPosition[1],currentPosition[2]+1]
		data[currentPosition[1],currentPosition[2]+1] = -2
		screen <- plotLabyrinth(data)
		pathFound=matrikaSosedov(data)
		data[currentPosition[1],currentPosition[2]+1] = tempInt
	}
	if (gor == -3) return(TRUE)
	if (!pathFound && gor > 0){
		tempInt=data[currentPosition[1]-1,currentPosition[2]]
		data[currentPosition[1]-1,currentPosition[2]] = -2
		screen <- plotLabyrinth(data)
		pathFound=matrikaSosedov(data)
		data[currentPosition[1]-1,currentPosition[2]] = tempInt
	}
	if (levo == -3) return(TRUE)
	if (!pathFound && levo > 0){
		tempInt=data[currentPosition[1],currentPosition[2]-1]
		data[currentPosition[1],currentPosition[2]-1] = -2
		screen <- plotLabyrinth(data)
		pathFound=matrikaSosedov(data)
		data[currentPosition[1],currentPosition[2]-1] = tempInt
	}
	return(pathFound)
}


data <- read.table("labyrinth_1.txt", sep=",", header=F)
data <- as.matrix(data)
screen <- plotLabyrinth(data)

matrikaSosedov(data)









