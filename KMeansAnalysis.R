source("k-means/KMeansAlgorithm.R")

# CREATE SYNTHETIC DATA
set.seed(105)  # Set seed for reproducible results

# 4 multivariate normal distributions with equal variance
mu1 = c(10, 10)
mu2 = c(30, 10)
mu3 = c(25, 40)
mu4 = c(25, 25)
sigma = 16*diag(2)

group1 = rmnorm(n = 100, mu1, sigma)
group2 = rmnorm(n = 100, mu2, sigma)
group3 = rmnorm(n = 100, mu3, sigma)
group4 = rmnorm(n = 100, mu4, sigma)

plot(group1, xlim=range(0,60), ylim=range(0,60), main="Simulated Data", xlab="X1", ylab="X2", col="red")
points(group2, col="blue")
points(group3, col="yellow")
points(group4, col="green")

data = rbind(group1, group2, group3, group4)

km = KMeansAlgorithm(x = data, k = 4, iter.max = 10, nstart = 3)  # Create K-Means object
km$start()

test_data = c()
for (index in 1:400){
  if(km$group_allocation[index] == 3){
    test_data = rbind(test_data, data[index,])
  }
}
points(test_data[,1], test_data[,2])

km.r <- kmeans(data, 4, iter.max = 10)  # K-Means R package to compare
print(km.r)
print(km)

print(km$cluster_means)
print(km.r$centers)