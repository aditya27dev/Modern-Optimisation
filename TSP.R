library(tidyverse)
library(ggrepel)
library(GA)
library(readr)
library(readxl)

getData <- function(){
  route <<- read_excel("rc106.xls")
  route_distance <<- dist(route[ , 2:3], upper = T, diag = T) %>% as.matrix()
  D <- as.matrix(route_distance)
  return(D)
}

tourLength <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])             #e.g. convert A,B,C to A,B,C,A. Thus the tour finishes where it started.
  route <- embed(tour, 2)[,2:1]        #converts the tour into a matrix of trips. i.e. 
  # 
  print(tour)
  tourlength <- sum(distMatrix[route]) #tour length must be minimised
  return(tourlength)                   #however, GA package only maximises. So 1/tourlength can be maximised. 
}

tspFitness <- function(x, distance, ...){
  
  visited_node <- 1
  
  for (i in x) {
    
    initial_node <- i
    visited_node <- c(visited_node, initial_node)
    
  }
  
  visited_node <- c(visited_node, 1)
  total_distance <- embed(visited_spot, 2)[ , 2:1] %>% distance[.] %>% sum()
  
  return(-total_distance)
}

plotTSPSolution<-function(solution){
  data("eurodist", package = "datasets")
  mds <- cmdscale(route)
  x <- mds[, 1]
  y <- -mds[, 2]
  plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
  abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
         col = "light gray")
  tour <- solution[1, ]
  tour <- c(tour, tour[1])
  n <- length(tour)
  arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
         length = 0.15, angle = 25, col = "steelblue", lwd = 2)
  text(x, y, labels(route), cex=0.8)
}

tspresult1 = runGA(popSize = 100, crossover = gaperm_pmxCrossover, problem = "tsp")
bestFitnesstsp1 = getBestFitness()
bestSoltsp1 = getBestSolution()
tspresult2 = runGA(popSize = 100, crossover = gaperm_oxCrossover, problem = "tsp")
bestFitnesstsp2 = getBestFitness()
bestSoltsp2 = getBestSolution()
tspresult3 = runGA(popSize = 100, crossover = gaperm_pbxCrossover, problem = "tsp")
bestFitnesstsp3 = getBestFitness()
bestSoltsp3 = getBestSolution()
p1 <- parseData(tspresult1, 2, 30)
p2 <- parseData(tspresult2, 2, 30)
p3 <- parseData(tspresult3, 2, 30)

plotbars(p1, p2, p3, "Crossover = gaperm_pmxCrossover", "Crossover = gaperm_oxCrossover", "Crossover = gaperm_pbxCrossover")
