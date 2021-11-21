library("mnormt") # R package for multivariate normal distribution

KMeansAlgorithm = setRefClass("KMeansAlgorithm", fields = list(x = "matrix", k = "numeric", iter.max = "numeric", nstart = "numeric", groups = "list", group_allocation = "vector"),
                              methods = list(
                                
                                # Initialises and runs the K-Means algorithm
                                start = function() {
                                  groups <<- vector(mode = "list", length = k)
                                  
                                  # Initial assignment probability into k groups is 1/k
                                  group_allocation <<- sample(1:(k), nrow(x), replace=T)
                                  
                                  for(i in 1:(nrow(x))){  # Each data point index will be assigned to each group
                                    groups[[group_allocation[i]]] <<- c(groups[[group_allocation[i]]], i)
                                  }
                                  
                                  for(iteration in 1:iter.max){
                                    update()
                                  }
                                },
                                
                                # Updates the group assignments
                                update = function() {
                                  dimension = dim(data)[2]
                                  # Estimate group means
                                  
                                  for(i in 1:(nrow(x))){  # Update sets of clusters with the ID's that are currently assigned to that cluster
                                    groups[[group_allocation[i]]] <<- c(groups[[group_allocation[i]]], i)
                                  }
                                  
                                  mu_hat = matrix(list(), nrow=k, ncol=dimension)
                                  for(i in 1:k){
                                    current_group_data = c()
                                    for(j in groups[[i]]){
                                      current_group_data = rbind(current_group_data, x[j,])
                                    }
                            
                                    mu_hat[i,] = (1/length(groups[[i]]))*colSums(current_group_data)
                                  }
                                  
                                  for (i in 1:(nrow(x))){  # Update group assignment
                                    closest_group = 1 # CHANGE THIS LATER
                                    closest_distance = 10000
                                    for (group_number in 1:k) {
                                      if(norm(x[i,] - unlist(mu_hat[group_number,]), type="2") < closest_distance){
                                        
                                        closest_group = group_number
                                        closest_distance = norm(x[i,] - unlist(mu_hat[group_number,]), type="2")
                                      }
                                    
                                    }
                                    group_allocation[i] <<- closest_group
                                  }
                                }
                                )
)

# CREATE SYNTHETIC DATA
set.seed(123)  # Set seed for reproducible results

mu1 = c(10, 10)
mu2 = c(30, 10)
mu3 = c(25, 40)
mu4 = c(25, 25)
sigma = 6*diag(2)

group1 = rmnorm(n = 100, mu1, sigma)
group2 = rmnorm(n = 100, mu2, sigma)
group3 = rmnorm(n = 100, mu3, sigma)
group4 = rmnorm(n = 100, mu4, sigma)

plot(group1, xlim=range(-10,50), ylim=range(-10,80), main="Simulated Data", xlab="X1", ylab="X2", col="red")
points(group2, col="blue")
points(group3, col="yellow")
points(group4, col="green")

data = rbind(group1, group2, group3, group4)

a = KMeansAlgorithm(x = data, k = 4, iter.max = 100, nstart = 3)
a$start()

test_data = c()
for (index in 1:400){
  if(a$group_allocation[index] == 1){
    test_data = rbind(test_data, data[index,])
  }
}
points(test_data[,1], test_data[,2])

