# Rough k-means clustering algorithm, written by Mateusz Szymonski 2020

# ------------------------ Auxiliary Functions -------------------------

SquaredEuclideanDistance <- function(p1, p2) {
  return(sum((p1 - p2)^2))
}

UpdateCentroids <- function(data, cluster.number, centroids, approximation.lower, approximation.upper, weight.lower) {
  approximation.difference = approximation.upper - approximation.lower
  
  for(i in 1:cluster.number)
  {
    cluster.approximation.lower.empty = !any(approximation.lower[,i] == 1)
    cluster.approximation.difference.empty  = !any(approximation.difference[,i] == 1)
    
    if (cluster.approximation.lower.empty == FALSE && cluster.approximation.difference.empty == TRUE)
    {
      cluster.data.rows = which(approximation.lower[,i] == 1)
      centroids[i,] = c(mean(data[cluster.data.rows,1]), mean(data[cluster.data.rows,2]))
    }
    else if (cluster.approximation.lower.empty == TRUE && cluster.approximation.difference.empty == FALSE)
    {
      cluster.difference.data.rows = which(approximation.difference[,i] == 1)
      centroids[i,] = c(mean(data[cluster.difference.data.rows,1]), mean(data[cluster.difference.data.rows,2]))
    }
    else
    {
      cluster.data.rows = which(approximation.lower[,i] == 1)
      cluster.difference.data.rows = which(approximation.difference[,i] == 1)
      centroids[i,] = weight.lower * c(mean(data[cluster.data.rows,1]), mean(data[cluster.data.rows,2])) + (1-weight.lower) * c(mean(data[cluster.difference.data.rows,1]), mean(data[cluster.difference.data.rows,2]))
    }
  }
  
  return (centroids)
}

RecalculateClusters <- function(data, cluster.number, centroids, approximation.lower, approximation.upper, epsilon) {
  
  data.number = length(data[,1])
  
  distance.matrix <- matrix(NA, nrow = data.number, ncol=cluster.number)
  for(i in 1:data.number) {
    for (j in 1:cluster.number) {
      distance.matrix[i,j] = SquaredEuclideanDistance(data[i,], centroids[j,])
    }
  }
  
  for(i in 1:data.number) {
    
    cluster.indices <- 1:cluster.number
    minimal.distance.index = which(distance.matrix[i,] == min(distance.matrix[i,]))[1]
    not.minimal.distance.index = cluster.indices[-minimal.distance.index] # Remove index of minimal element
    
    approximation.lower[i,] = rep(0, cluster.number) # zero out assignments
    approximation.upper[i,] = rep(0, cluster.number) # zero out assignments
    
    for (j in 1:length(not.minimal.distance.index)) # For each possible cluster (Except closest one)
    {
      assignment.factor = distance.matrix[i, not.minimal.distance.index[j]] / distance.matrix[i, minimal.distance.index]

      if (!(assignment.factor <= epsilon)) 
      {
        if (approximation.upper[i, minimal.distance.index] == 0) { # If is already assigned to upper of the closest one
          approximation.upper[i,] = rep(0, cluster.number) # zero out assignments
          
          approximation.lower[i, minimal.distance.index] = 1 # assign
          approximation.upper[i, minimal.distance.index] = 1 # assign
        }
      }
      else
      {
        approximation.lower[i,] = rep(0, cluster.number) # zero out assignments
        
        approximation.upper[i, not.minimal.distance.index[j]] = 1 # assign to both tested
        approximation.upper[i, minimal.distance.index] = 1 # assign to both tested
      }
    }
  }
  
  updated.approximations <- list("approximation.lower" = approximation.lower, "approximation.upper" = approximation.upper)
  
  return (updated.approximations)
}

# ----------------------------- Algorithm ------------------------------

RoughKMeans <- function(data, cluster.number = 2, epsilon = 1.5, weight.lower = 0.7, iteration.limit = 100) {
  
  # --- Validate parameters
  if (length(data[,1]) < 10) {
    stop("Error: Invalid data. More data points required")
  }
  if (length(data[1,]) < 2) {
    stop("Error: Invalid data. Two parameters per data point required (X and Y)")
  }
  if (cluster.number < 1) {
    stop("Error: Invalid cluster.number. Must be greater than 0")
  }
  if (epsilon < 1){
    stop("Error: Invalid epsilon. Must be greater or equal 1")
  }
  if (weight.lower < 0 || weight.lower > 1){
    stop("Error: Invalid weight.lower. Must be in range [0,1]")
  }
  if (iteration.limit <= 0){
    stop("Error: Invalid iteration.limit. Must be greater than 0")
  }

  data <- data[,c(1:2)]
  data.number = length(data[,1])
  
  message("Calculating clusters...")
  
  centroids <- matrix(NA, nrow = cluster.number, ncol=2)
  
  # --- Randomly assign data to clusters
  
  approximation.lower <- matrix(0, nrow = data.number, ncol=cluster.number)
  approximation.upper <- matrix(0, nrow = data.number, ncol=cluster.number)
  
  for(i in 1:data.number) {
    random.cluster <- sample(1:cluster.number, 1)
    approximation.lower[i,random.cluster] = 1
  }
  approximation.upper = approximation.lower
  
  # --- Repeat updating centroids and recalculating clusters
  
  counter <- 0
  repeat{
    previous.approximation.upper = approximation.upper
    
    centroids = UpdateCentroids(data, cluster.number, centroids, approximation.lower, approximation.upper, weight.lower)
    
    updated.approximations = RecalculateClusters(data, cluster.number, centroids, approximation.lower, approximation.upper, epsilon)
    approximation.lower = updated.approximations$approximation.lower
    approximation.upper = updated.approximations$approximation.upper
    
    identifier <- sum(abs(previous.approximation.upper - approximation.upper))
    counter <- counter + 1
    if (identifier == 0) { break }
    if (counter == iteration.limit) { break }
  }
  
  roughKMeansResult <- list(
    lowerApprox = approximation.lower,
    upperApprox = approximation.upper,
    centers = centroids,
    iterations = counter
  )
  
  class(roughKMeansResult) <- "RoughKMeansResult"
  return (roughKMeansResult)
}

