source("k-means/KMeansAlgorithm.R")
library("mnormt") # R package for multivariate normal distribution

set.seed(158)  # Set seed for reproducible results
# Data 1 4 multivariate normal distributions with equal variance
mu1 = c(50, 10)
mu2 = c(30, 90)
mu3 = c(15, 40)
mu4 = c(80, 40)
sigma = 50*diag(2)

group1 = rmnorm(n = 50, mu1, sigma)
group2 = rmnorm(n = 50, mu2, sigma)
group3 = rmnorm(n = 50, mu3, sigma)
group4 = rmnorm(n = 50, mu4, sigma)

plot(group1, xlim=range(-20,120), ylim=range(-20,120), main="Simulated Data Set 1", xlab="X1", ylab="X2", col="red")
points(group2, col="blue")
points(group3, col="orange")
points(group4, col="green")
legend("topright", title = "Cluster Colours",legend= c("Group A", "Group B", "Group C", "Group D")
       ,fill = c("red", "blue", "orange", "green"), cex = 0.6)
data = rbind(group1, group2, group3, group4)


# Create K-Means object from implementation in KMeansAlgorithm.R
km = KMeansAlgorithm(x = data, k = 4, iter.max = 20, nstart = 3,single_point_initialisation = FALSE)  
km$start()  # Initalise algorithm

cluster_allocation = rep(c("a","b","c","d"),each=50)

print("OUR IMPLEMENTAION DATA 1")
cat("between_SS / total_SS = ", (km$betweenss/(km$tot.withinss + km$betweenss)*100), "%")
km$cluster_means
print("Cluster assignment matrix:")
table(cluster_allocation, km$group_allocation)
cat("Number of iterations:", km$iter)


print("R IMPLEMENTAION DATA 1")
km.r <- kmeans(data, 4, iter.max = 20)  # K-Means R package to compare
cat("between_SS / total_SS = ", (km.r$betweenss/(km.r$tot.withinss + km.r$betweenss)*100), "%")
km.r$centers
print("Cluster assignment matrix:")
table(cluster_allocation, km.r$cluster)
cat("Number of iterations:", km.r$iter)


print("OUR IMPLEMENTAION DATA 1 SINGLE POINT CENTRES")
km = KMeansAlgorithm(x = data, k = 4, iter.max = 20, nstart = 3,single_point_initialisation = TRUE)  
km$start()  # Initalise algorithm

cat("between_SS / total_SS = ", (km$betweenss/(km$tot.withinss + km$betweenss)*100), "%")
km$cluster_means
print("Cluster assignment matrix:")
table(cluster_allocation, km$group_allocation)
cat("Number of iterations:", km$iter)

# Data 2
mu1 = c(10, 10)
mu2 = c(40, 10)
mu3 = c(80, 80)
mu4 = c(0, 90)
sigma = 80*diag(2)

group1 = rmnorm(n = 50, mu1, sigma)
group2 = rmnorm(n = 50, mu2, sigma)
group3 = rmnorm(n = 50, mu3, sigma)
group4 = rmnorm(n = 50, mu4, sigma)

plot(group1, xlim=range(-20,120), ylim=range(-20,120), main="Simulated Data Set 2", xlab="X1", ylab="X2", col="red")
points(group2, col="blue")
points(group3, col="orange")
points(group4, col="green")
legend("topright", title = "Cluster Colours",legend= c("Group A", "Group B", "Group C", "Group D")
       ,fill = c("red", "blue", "orange", "green"), cex = 0.6)
data_2 = rbind(group1, group2, group3, group4)

try({  # Catch the error thrown when a cluster is empty
# Create K-Means object from implementation in KMeansAlgorithm.R
km = KMeansAlgorithm(x = data_2, k = 4, iter.max = 20, nstart = 3, single_point_initialisation = FALSE)  
km$start()  # Initalise algorithm
}, {print("Error, a cluster is empty")}, silent = FALSE)

km = KMeansAlgorithm(x = data_2, k = 4, iter.max = 20, nstart = 3, single_point_initialisation = TRUE)  
km$start()  # Initalise algorithm

print("OUR IMPLEMENTAION DATA 2")
cat("between_SS / total_SS = ", (km$betweenss/(km$tot.ss)*100), "%")
km$cluster_means
print("Cluster assignment matrix:")
table(cluster_allocation, km$group_allocation)
cat("Number of iterations:", km$iter)


print("R IMPLEMENTAION DATA 2")
km.r <- kmeans(data, 4, iter.max = 20)  # K-Means R package to compare
cat("between_SS / total_SS = ", (km.r$betweenss/(km.r$totss)*100), "%")
km.r$centers
print("Cluster assignment matrix:")
table(cluster_allocation, km.r$cluster)
cat("Number of iterations:", km.r$iter)

colour_list = c("red", "blue", "orange", "green")
# PLOT OUR RESULT
plot(c(), xlim=range(-20,120), ylim=range(-20,120), main="Our K-Means Implementation Data Set 2", xlab="X1", ylab="X2", col="red")
for (index in 1:200){
  colour = colour_list[km$group_allocation[index]]
  points(data_2[index,1], data_2[index, 2], col = colour)
}

# PLOT R RESULT
plot(c(), xlim=range(-20,120), ylim=range(-20,120), main="R K-Means Implementation Data Set 2", xlab="X1", ylab="X2", col="red")
for (index in 1:200){
  colour = colour_list[km.r$cluster[index]]
  points(data_2[index,1], data_2[index, 2], col = colour)
}
