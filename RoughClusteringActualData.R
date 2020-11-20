library(DescTools)
library(ggplot2)
library(ggpubr)
library(gridExtra)

rm(list = ls())
source("RoughKMeans.R")

# --- Load data sets
db.file.PWT = "./Data/PWT/pwt91.csv"
db.PWT <- read.csv(db.file.PWT)
db.PWT <- db.PWT[c("country", "year", "pop", "rgdpna", "rdana")]

db.file.Swiid = "./Data/Swiid/swiid8_3_summary.csv"
db.Swiid <- read.csv(db.file.Swiid)
db.Swiid <- db.Swiid[c("country", "year", "gini_mkt", "gini_disp")]

db.needed <- merge(db.PWT, db.Swiid, by = c("country", "year")) # Merge two databases

db.needed.prepared <- data.frame()
countries <- c("United States", "Mexico", "Chile", "Turkey", "Germany", "Poland", "Czech Republic", "Sweden")
for(i in 1:length(countries)) {
  db.country.data <- db.needed[db.needed$country == countries[i],] # Get rows for only one country
  db.country.data <- db.country.data[complete.cases(db.country.data), ] # Remove rows with NA

  time.range <- c(min(db.country.data$year), max(db.country.data$year))

  growth.rate.factor <- ts(log(db.country.data$rgdpna/db.country.data$pop), start=c(time.range[1]), end=c(time.range[2]), frequency=1)
  growth.rate <- as.numeric(-100 * ((growth.rate.factor) -  stats::lag(growth.rate.factor)))

  gini.mkt.rate.factor <- ts(log(db.country.data$gini_mkt), start=c(time.range[1]), end=c(time.range[2]), frequency=1)
  gini.mkt.rate <- as.numeric(-100 * (gini.mkt.rate.factor - stats::lag(gini.mkt.rate.factor)))

  gini.disp.rate.factor <- ts(log(db.country.data$gini_disp), start=c(time.range[1]), end=c(time.range[2]), frequency=1)
  gini.disp.rate <- as.numeric(-100 * (gini.disp.rate.factor - stats::lag(gini.disp.rate.factor)))

  pop.rate.factor <- ts(log(db.country.data$pop), start=c(time.range[1]), end=c(time.range[2]), frequency=1)
  pop.rate <- as.numeric(-100 * ((pop.rate.factor) - stats::lag(pop.rate.factor)))

  db.country.data <- db.country.data[-1,] # Remove first row
  db.country.data <- cbind(db.country.data, growth.rate) # Add column
  db.country.data <- cbind(db.country.data, gini.mkt.rate) # Add column
  db.country.data <- cbind(db.country.data, gini.disp.rate) # Add column
  db.country.data <- cbind(db.country.data, pop.rate) # Add column
  db.needed.prepared <- rbind(db.needed.prepared, db.country.data) # Add country data to common dataframe
}

# --- Prepare data

data <- db.needed.prepared
cluster.number <- 4

x <- data$pop.rate
y <- data$gini.mkt.rate
color <- data$country
title <- ""
x.label <- "Population Growth Rate"
y.label <- "Gini Market Rate"

# --- Plot
t <- ggplot() +
  geom_point(data=data, aes(x=x, y=y, color=country), size = 1) +
  labs(color = "Countries", title=title, y=y.label, x=x.label) + theme(legend.position="right") +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  scale_color_manual(values = c(2:500), name="Countries")

print(t)

#ggsave(t, file=paste0("Plots/Actual_Noclusters",".png"), width = 15, height = 13, units = "cm")

# --- Calculate clusters
data <- data[c("pop.rate", "gini.mkt.rate", "country")]
result <- RoughKMeans(data = data, cluster.number = cluster.number, epsilon = 1.5, weight.lower = 0.7, iteration.limit = 50)

# --- Plot
p <- ggplot()

# --- lowerApprox
lowerApprox.indices = which(rowSums(result$lowerApprox) == 1)
lowerApprox.pure.data = data[lowerApprox.indices,,drop = FALSE]
lowerApprox.pure <- result$lowerApprox[lowerApprox.indices,,drop = FALSE]
lowerApprox.clusters <- rep(0, length(lowerApprox.pure[,1]))

if (length(lowerApprox.pure[,1]) > 0) {
  for(i in 1:length(lowerApprox.pure[,1])) {
    for(j in 1:length(result$centers[,1])) {
      if(lowerApprox.pure[i,j] == 1) {
        lowerApprox.clusters[i] = j
      }
    }
  }
}

color <- as.factor(lowerApprox.clusters)
p = p + geom_point(data=data, aes(x=x, y=y, color=country), pch=16)
p = p + stat_chull(data=lowerApprox.pure.data, aes(x=x[lowerApprox.indices], y=y[lowerApprox.indices], fill=color), alpha = 0.13, geom = "polygon") 

# --- upperApprox\lowerApprox (pure upperApprox)
approx.difference.indices = which(rowSums(result$upperApprox) > 1)
approx.difference.pure.data = data[approx.difference.indices,,drop = FALSE]
approx.difference.pure <- result$upperApprox[approx.difference.indices,,drop = FALSE]
approx.difference.clusters <- rep(0, length(approx.difference.pure[,1]))

if (length(approx.difference.pure[,1]) > 0) {
  for(i in 1:length(approx.difference.pure[,1])) {
    type = 0
    for(j in 1:length(result$centers[,1])) {
      if(approx.difference.pure[i,j] == 1) {
        type = type + j
      }
    }
    approx.difference.clusters[i] = type
  }
}

color.difference <- as.factor(8)
shape.difference <- as.factor(approx.difference.clusters-2)
p = p + geom_point(data=approx.difference.pure.data, aes(x=x[approx.difference.indices], y=y[approx.difference.indices], pch=shape.difference), color=color.difference, size=3)

# Here is some weird bug: matrices are being reasinged each loop but stat_chull detects 
# them as they are the same (not updated), x and y length don't match data length and stat_chull won't generate hull
# It is interesting because same thing in RoughClusteringSynthesizedData.R works without problems
# --- upperApprox
# for(i in 1:length(result$centers[,1])) { # All in this (also shared)
#   upperApprox.indices = which(result$upperApprox[,i] > 0)
#   upperApprox.pure.data = data[upperApprox.indices,,drop = FALSE]
#   upperApprox.pure = result$upperApprox[upperApprox.indices,,drop = FALSE]
#   upperApprox.clusters = rep(i, length(upperApprox.pure[,1]))
#   p = p + stat_chull(data=upperApprox.pure.data, aes(x=x[upperApprox.indices], y=y[upperApprox.indices]), fill=i+1, color=i+1, alpha = 0.08, geom = "polygon")
# }

# Super ugly hack
switch(length(result$centers[,1]),  
   {
     p = p + stat_chull(data=data[which(result$upperApprox[,1] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,1] > 0)], y=y[which(result$upperApprox[,1] > 0)]), fill=1+1, color=1+1, alpha = 0.08, geom = "polygon")
   },
   {
     p = p + stat_chull(data=data[which(result$upperApprox[,1] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,1] > 0)], y=y[which(result$upperApprox[,1] > 0)]), fill=1+1, color=1+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,2] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,2] > 0)], y=y[which(result$upperApprox[,2] > 0)]), fill=2+1, color=2+1, alpha = 0.08, geom = "polygon")
   },
   {
     p = p + stat_chull(data=data[which(result$upperApprox[,1] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,1] > 0)], y=y[which(result$upperApprox[,1] > 0)]), fill=1+1, color=1+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,2] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,2] > 0)], y=y[which(result$upperApprox[,2] > 0)]), fill=2+1, color=2+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,3] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,3] > 0)], y=y[which(result$upperApprox[,3] > 0)]), fill=3+1, color=3+1, alpha = 0.08, geom = "polygon")
   },
   {
     p = p + stat_chull(data=data[which(result$upperApprox[,1] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,1] > 0)], y=y[which(result$upperApprox[,1] > 0)]), fill=1+1, color=1+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,2] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,2] > 0)], y=y[which(result$upperApprox[,2] > 0)]), fill=2+1, color=2+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,3] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,3] > 0)], y=y[which(result$upperApprox[,3] > 0)]), fill=3+1, color=3+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,4] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,4] > 0)], y=y[which(result$upperApprox[,4] > 0)]), fill=4+1, color=4+1, alpha = 0.08, geom = "polygon")
   },
   {
     p = p + stat_chull(data=data[which(result$upperApprox[,1] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,1] > 0)], y=y[which(result$upperApprox[,1] > 0)]), fill=1+1, color=1+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,2] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,2] > 0)], y=y[which(result$upperApprox[,2] > 0)]), fill=2+1, color=2+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,3] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,3] > 0)], y=y[which(result$upperApprox[,3] > 0)]), fill=3+1, color=3+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,4] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,4] > 0)], y=y[which(result$upperApprox[,4] > 0)]), fill=4+1, color=4+1, alpha = 0.08, geom = "polygon")
     p = p + stat_chull(data=data[which(result$upperApprox[,5] > 0),,drop = FALSE], aes(x=x[which(result$upperApprox[,5] > 0)], y=y[which(result$upperApprox[,5] > 0)]), fill=5+1, color=5+1, alpha = 0.08, geom = "polygon")
   },
)

# --- centroids
color.centers <- as.factor(c(1:length(result$centers[,1])+1))
p = p + geom_point(data=as.data.frame(result$centers), aes(x=result$centers[,1], y=result$centers[,2]), color=color.centers, size=7, shape=8)

# --- print
p = p + 
  scale_color_manual(values = c(2:500), name="Countries") + 
  scale_shape_manual(values = c(2:500), name="Upper\\Lower\nApproximations") +
  scale_fill_manual(values = c(2:500), name="Clusters") +
  labs(title=title, y=y.label, x=x.label) + 
  theme(legend.position="right")

print(p)

#ggsave(p, file=paste0("Plots/Actual_clusters_", cluster.number,".png"), width = 15, height = 13, units = "cm")

message("Number of iterations: ", result$iterations)

