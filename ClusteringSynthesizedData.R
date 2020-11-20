library(DescTools)
library(ggplot2)
library(ggpubr)
library(gridExtra)


rm(list = ls())
source("CustomKMeans.R")


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
for (i in 1:length(data.type)) {
  data <- data.type[[i]]
  cluster.number <- 3
  
  # --- Calculate clusters
  result <- CustomKMeans(data, cluster.number, 20)
  
  x <- data$x
  y <- data$y
  color <- as.factor(result$cluster)
  title <- ""
  x.label <- "X"
  y.label <- "Y"
  
  # --- Plot
  p <- ggplot() + 
    geom_point(data=data, aes(x=x, y=y, color=color), show.legend = F) +
    geom_point(data=as.data.frame(result$centers), aes(x=result$centers[,1], y=result$centers[,2]), size=5, shape=8) +
    stat_chull(data=data, aes(x=x, y=y, fill=color), alpha = 0.1, geom = "polygon") + 
    labs(fill = "Clusters", title=title, y=y.label, x=x.label) + theme(legend.position="right") +
    guides(colour = guide_legend(override.aes = list(size=3)))

  ggsave(p, file=paste0("Plots/Synthesized_", i, "_clusters_", cluster.number,".png"), width = 15, height = 13, units = "cm")

  print(p)
  
  message("Number of iterations: ", result$iterations)
}
