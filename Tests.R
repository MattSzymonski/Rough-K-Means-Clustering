library(DescTools)
library(ggplot2)
library(ggpubr)
library(gridExtra)

rm(list = ls())
source("RoughKMeans.R")


# --- Load data sets
db.file.MickeyMouse = "./Data/Custom/MickeyMouse.csv"
db.MickeyMouse <- read.csv(db.file.MickeyMouse)

data <- db.MickeyMouse
cluster.number <- 3






result = RoughKMeans(data=data, cluster.number = cluster.number, iteration.limit = 10, epsilon = 1.5, weight.lower = 0.7)
#result



x <- data$x
y <- data$y
title <- ""
x.label <- "X"
y.label <- "Y"

p <- ggplot()

# --- lowerApprox
lowerApprox.indices = which(rowSums(result$lowerApprox) == 1)
lowerApprox.pure.data = data[lowerApprox.indices,]
lowerApprox.pure <- result$lowerApprox[lowerApprox.indices,]
lowerApprox.clusters <- rep(0, length(lowerApprox.pure[,1]))

for(i in 1:length(lowerApprox.pure[,1])) {
  for(j in 1:2) {
    if(lowerApprox.pure[i,j] == 1) {
      lowerApprox.clusters[i] = j
    }
  }
}

color <- as.factor(lowerApprox.clusters)
p = p + geom_point(data=lowerApprox.pure.data, aes(x=x, y=y, color=color), show.legend = T)
p = p + stat_chull(data=lowerApprox.pure.data, aes(x=x, y=y, fill=color), alpha = 0.1, geom = "polygon") 

# --- upperApprox - lowerApprox (pure upperApprox)
approx.difference.indices = which(rowSums(result$upperApprox) > 1)
approx.difference.pure.data = data[approx.difference.indices,]
approx.difference.pure <- result$upperApprox[approx.difference.indices,]
approx.difference.clusters <- rep(0, length(approx.difference.pure[,1]))

for(i in 1:length(approx.difference.pure[,1])) {
  for(j in 1:2) {
    if(approx.difference.pure[i,j] == 1) {
      approx.difference.clusters[i] = j
    }
  }
}

color.difference <- as.factor(8)
p = p + geom_point(data=approx.difference.pure.data, aes(x=x, y=y), color=color.difference, pch=17)

# --- upperApprox 
for(i in 1:length(result$centers[,1])) # All in this (also shared)
{
  upperApprox.indices = which(result$upperApprox[,i] > 0)
  upperApprox.pure.data = data[upperApprox.indices,]
  upperApprox.pure <- result$upperApprox[upperApprox.indices,]
  upperApprox.clusters <- rep(i, length(upperApprox.pure[,1]))
  
  p = p + stat_chull(data=upperApprox.pure.data, aes(x=x, y=y), color=i+1, alpha = 0.0, geom = "polygon") 
}

# --- centroids
color.centers <- as.factor(c(1:length(result$centers[,1])+1))
p = p + geom_point(data=as.data.frame(result$centers), aes(x=result$centers[,1], y=result$centers[,2]), color=color.centers, size=5, shape=8)

# --- print
p = p + scale_color_manual(values = c(2:100)) + scale_fill_manual(values = c(2:100)) 
print(p)

message("Number of iterations: ", result$iterations)
