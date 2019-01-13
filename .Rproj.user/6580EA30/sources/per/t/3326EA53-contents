source("modifyM.R")
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


mainMeja <- function(data,myLvl,maxLvl){
	if (myLvl >= maxLvl) return(FALSE)
	currentPosition=which(data == -2,TRUE)
	data[currentPosition[1],currentPosition[2]]=-4	

	dol=data[currentPosition[1]+1,currentPosition[2]]
	desno=data[currentPosition[1],currentPosition[2]+1]
	gor=data[currentPosition[1]-1,currentPosition[2]]
	levo=data[currentPosition[1],currentPosition[2]-1]
	
	x=c(dol,desno,gor,levo)
	if (any(x==-3)){
		data[currentPosition[1]+1,currentPosition[2]] = -4
		screen <- plotLabyrinth(data)
		return(data)
	}

	pathFound=FALSE
	if (dol >= 0){
		tempInt=data[currentPosition[1]+1,currentPosition[2]]
		data[currentPosition[1]+1,currentPosition[2]] = -2
		screen <- plotLabyrinth(data)
		pathFound=mainMeja(data,myLvl+1,maxLvl)
		data[currentPosition[1]+1,currentPosition[2]] = tempInt
	}
	if (!is.matrix(pathFound) && desno >= 0){
		tempInt=data[currentPosition[1],currentPosition[2]+1]
		data[currentPosition[1],currentPosition[2]+1] = -2
		screen <- plotLabyrinth(data)
		pathFound=mainMeja(data,myLvl+1,maxLvl)
		data[currentPosition[1],currentPosition[2]+1] = tempInt
	}
	if (!is.matrix(pathFound) && gor >= 0){
		tempInt=data[currentPosition[1]-1,currentPosition[2]]
		data[currentPosition[1]-1,currentPosition[2]] = -2
		screen <- plotLabyrinth(data)
		pathFound=mainMeja(data,myLvl+1,maxLvl)
		data[currentPosition[1]-1,currentPosition[2]] = tempInt
	}
	if (!is.matrix(pathFound) && levo >= 0){
		tempInt=data[currentPosition[1],currentPosition[2]-1]
		data[currentPosition[1],currentPosition[2]-1] = -2
		screen <- plotLabyrinth(data)
		pathFound=mainMeja(data,myLvl+1,maxLvl)
		data[currentPosition[1],currentPosition[2]-1] = tempInt
	}
	return(pathFound)
}

meja <- function(data){
	i=1
	rez=FALSE
	while (!is.matrix(rez)){
		rez=mainMeja(data,0,i)
		i=i+6
	}
	return(rez)
}

getPath<- function(data,cp){
	pathData=cp
	dol=data[cp[1]+1,cp[2]]
	desno=data[cp[1],cp[2]+1]
	gor=data[cp[1]-1,cp[2]]
	levo=data[cp[1],cp[2]-1]

	x=c(dol,desno,gor,levo)
	
	if(dol == -4)
		cp[1]=cp[1]+1
	else if(desno == -4)
		cp[2]=cp[2]+1
	else if(gor == -4)
		cp[1]=cp[1]-1
	else if(levo == -4)
		cp[2]=cp[2]-1
	if (!any(x==-4)){
		data[pathData[1],pathData[2]]=-5
		if(dol == -3)
			cp[1]=cp[1]+1
		else if(desno == -3)
			cp[2]=cp[2]+1
		else if(gor == -3)
			cp[1]=cp[1]-1
		else if(levo == -3)
			cp[2]=cp[2]-1
		return(rbind(pathData,cp))
	}
	
	data[pathData[1],pathData[2]]=-5
	return(rbind(pathData,getPath(data,cp)))
}

getPoints<- function(data,matrix){
	points=0
	for(i in 3:nrow(matrix)-1){
		points=points+data[matrix[i,1],matrix[i,2]]	
	}
	return(points)
}

data <- read.table("labyrinth_11.txt", sep=",", header=F)
data <- as.matrix(data)
screen <- plotLabyrinth(data)

mx=meja(data)
a=getPath(mx,which(data == -2, TRUE))
getPoints(data,a)
print(paste("Stevilo vozlisc na poti:", nrow(a)))
setsOfCoords(a)
