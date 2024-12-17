# Creates the Clusters
create_clusters <- function(seed, k) {
  set.seed(seed);
  distances_data <- mall_data
  initial_centroids <- mall_data[sample(200, k, replace=FALSE), ]
  old_centroid_vals <- as.vector(matrix(0, nrow=k, ncol=2))
  new_centroid_vals <- as.vector(as.matrix(initial_centroids))
  new_centroids <- initial_centroids
  while (!all(old_centroid_vals == new_centroid_vals)) {
    old_centroid_vals <- new_centroid_vals
    for (i in 1:nrow(new_centroids)) {
      xDist <- sqrt((distances_data[1] - new_centroids[i, 1]) ^ 2)
      yDist <- sqrt((distances_data[2] - new_centroids[i, 2]) ^ 2)
      distances_data <- cbind(distances_data, (xDist + yDist))
    }
    names(distances_data) <- c("x", "y", paste0(rep("cluster", k), 1:k))
    cluster_match <- apply(distances_data[,c(-1, -2)], MARGIN=1, FUN=which.min)
    distances_data <- distances_data[, c(1,2)]
    new_data <- cbind(distances_data, cluster_match)
    new_means <- c()
    for (i in 1:length(unique(new_data$cluster_match))) {
      newMeanX <- mean(new_data[new_data$cluster_match == i,]$x)
      newMeanY <- mean(new_data[new_data$cluster_match == i,]$y)
      new_means <- c(new_means, c(newMeanX, newMeanY))
    }
    new_centroids <- matrix(new_means, ncol=2, byrow=TRUE)
    new_centroid_vals <- as.vector(new_centroids)
  }
  return(list(new_data, k))
}

# Calculating the Within-Cluster-Sum of Squared Errors
wss <- function(cluster) {
  k <- cluster[[2]]
  dist_total <- numeric(k)
  for (i in 1:k) {
    cluster_i <- mall_data[cluster[[1]]$cluster_match == i,]
    x_dist_total <- sum(sqrt((cluster_i[1] - mean(cluster_i$x)) ^ 2))
    y_dist_total <- sum(sqrt((cluster_i[2] - mean(cluster_i$y)) ^ 2))
    dist_total[i] <- x_dist_total + y_dist_total
  }
  return(sum(dist_total))
}