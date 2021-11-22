KMeansAlgorithm = setRefClass("KMeansAlgorithm", fields = list(x = "matrix", k = "numeric", iter.max = "numeric", nstart = "numeric", groups = "list", group_allocation = "vector", cluster_means = "matrix"),
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
                                  number_variables = dim(data)[2] # Number of variables for each data point
                                  
                                  groups <<- vector(mode = "list", length = k)
                                  for(i in 1:(nrow(x))){  # Update sets of clusters with the ID's that are currently assigned to that cluster
                                    groups[[group_allocation[i]]] <<- c(groups[[group_allocation[i]]], i)
                                  }
                                  
                                  # First estimate group means
                                  cluster_means <<- matrix(list(), nrow=k, ncol=number_variables)
                                  for(i in 1:k){  # Append the values of all the points currently in each cluster
                                    current_group_data = c()
                                    for(j in groups[[i]]){
                                      current_group_data = rbind(current_group_data, x[j,])
                                    }
                                    # Group mean estimators = (1/number of samples currently in the group)*(sum of variables)
                                    cluster_means[i,] <<- (1/length(groups[[i]]))*colSums(current_group_data)
                                  }
                                  
                                  for (i in 1:(nrow(x))){  # Check each data point for it's closest medioid
                                    closest_group = NULL
                                    closest_distance = 10000  # Arbitrary large value
                                    for (group_number in 1:k) {
                                      if(norm(x[i,] - unlist(cluster_means[group_number,]), type="2") < closest_distance){
                                        closest_group = group_number
                                        closest_distance = norm(x[i,] - unlist(cluster_means[group_number,]), type="2")
                                      }
                                    }
                                    group_allocation[i] <<- closest_group  # Update group assignment
                                  }
                                },
                                
                                results = function(){
                                  # Calculate within cluster and between cluster sum of squares
                                  # Cluster means and sizes
                                  
                                  # Total within group sum of squares
                                  tot.withinss = 0
                                  for(group_number in 1:k){
                                    for(i in groups[[group_number]]){
                                      tot.withinss = tot.withinss + t((x[i,] - unlist(cluster_means[group_number,])))%*%(x[i,] - unlist(cluster_means[group_number,]))
                                    }
                                  }
                                  print(tot.withinss)
                                  
                                  # Between group sum of squares
                                  betweenss = 0 
                                  global_mean = (1/nrow(x))*colSums(x)
                                  
                                  for(group_number in 1:k){
                                    betweenss = betweenss + length(groups[[group_number]])%*%t(unlist(cluster_means[group_number,]) - global_mean)%*%(unlist(cluster_means[group_number,]) - global_mean)
                                  }
                                  print(nrow(x))
                                  print(global_mean)
                                  print(betweenss)
                                  print("BETWEEN/ TOTAL SS =")
                                  print(betweenss/tot.withinss)
                                }
                                )
                            )


