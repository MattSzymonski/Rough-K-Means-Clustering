library(DescTools)
library(ggplot2)
library(ggpubr)
library(gridExtra)

rm(list = ls())
source("RoughKMeans.R")


# --- Load data sets
db.file.MickeyMouse = "./Data/Custom/MickeyMouse.csv"
db.MickeyMouse <- read.csv(db.file.MickeyMouse)

db.file.Circles = "./Data/Custom/Circles.csv"
db.Circles <- read.csv(db.file.Circles)

db.file.Crescents = "./Data/Custom/Crescents.csv"
db.Crescents <- read.csv(db.file.Crescents)

data.type <- list()
data.type[[1]] <- db.MickeyMouse
data.type[[2]] <- db.Circles
data.type[[3]] <- db.Crescents


# --- Prepare data
data.specifier <- 1 #length(data.type)
for (i in 1:data.specifier) {
  data <- data.type[[i]]
  cluster.number <- 3

  # --- Calculate clusters
  result = RoughKMeans(data=data, cluster.number = cluster.number, iteration.limit = 15, epsilon = 3.5, weight.lower = 0.8)
  
  # --- Plot
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
    for(j in 1:length(result$centers[,1])) {
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
    type = 0
    for(j in 1:length(result$centers[,1])) {
      if(approx.difference.pure[i,j] == 1) {
        type = type + j
      }
    }
    approx.difference.clusters[i] = type
  }

  #shapes <- c(21,22,23,24,56)
  color.difference <- as.factor(8)
  shape.difference <- as.factor(approx.difference.clusters)
  p = p + geom_point(data=approx.difference.pure.data, aes(x=x, y=y, pch=shape.difference), color=color.difference, size=2)
  
  # --- upperApprox 
  for(i in 1:length(result$centers[,1])) { # All in this (also shared)
    upperApprox.indices = which(result$upperApprox[,i] > 0)
    upperApprox.pure.data = data[upperApprox.indices,]
    upperApprox.pure <- result$upperApprox[upperApprox.indices,]
    upperApprox.clusters <- rep(i, length(upperApprox.pure[,1]))
    
    p = p + stat_chull(data=upperApprox.pure.data, aes(x=x, y=y), fill=i+1, color=i+1, alpha = 0.07, geom = "polygon") 
  }
  
  # --- centroids
  color.centers <- as.factor(c(1:length(result$centers[,1])+1))
  p = p + geom_point(data=as.data.frame(result$centers), aes(x=result$centers[,1], y=result$centers[,2]), color=color.centers, size=7, shape=8)
  
  # --- print
  p = p + 
    scale_color_manual(values = c(2:100)) + 
    scale_fill_manual(values = c(2:100)) +
    labs(fill = "Clusters", color = "Clusters", pch = "Boundary Elements", title=title, y=y.label, x=x.label) + 
    theme(legend.position="right")
  
  print(p)
  
  #ggsave(p, file=paste0("Plots/Synthesized_", i, "_clusters_", cluster.number,".png"), width = 15, height = 13, units = "cm")
  
  message("Number of iterations: ", result$iterations)
}
