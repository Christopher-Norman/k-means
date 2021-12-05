KMeansAlgorithm = setRefClass("KMeansAlgorithm", fields = list(x = "matrix", k = "numeric", iter.max = "numeric", nstart = "numeric", groups = "list", group_allocation = "vector", cluster_means = "matrix", iter = "numeric", tot.withinss = "numeric", betweenss = "numeric", tot.ss = "numeric", single_point_initialisation = "logical"),
  methods = list(
      # Initialises and runs the K-Means algorithm
      start = function() {
        groups <<- vector(mode = "list", length = k)  #  Holds  the k current  groups  andthe  respective  indexes  assigned  to them
        
        if(single_point_initialisation == TRUE){  # Random single data point centres
          number_variables = dim(x)[2] # Number of variables for each data point
          cluster_means <<- matrix(list(), nrow=k, ncol=number_variables)
          
          for(i in 1:k){
            cluster_means <<- x[sample(nrow(x),size=k,replace=FALSE),]
          }
          
          for (i in 1:(nrow(x))){  # Check each data point for it's closest cluster mean
            closest_group = NULL
            closest_distance = 10000000  # Arbitrary large value
            for (group_number in 1:k) {
              if(norm(x[i,] - unlist(cluster_means[group_number,]), type="2") < closest_distance){
                closest_group = group_number
                closest_distance = norm(x[i,] - unlist(cluster_means[group_number,]), type="2")
              }
            }
            group_allocation[i] <<- closest_group  # Update group assignment
          }
          
        } else {  # Randomly assign each data point to k groups 
          group_allocation <<- sample(1:(k), nrow(x), replace=T)
        
          for(i in 1:(nrow(x))){  # Each data point index will be assigned to each group
            groups[[group_allocation[i]]] <<- c(groups[[group_allocation[i]]], i)
          }
        }
        
        iter <<- 0
        for(iteration in 1:iter.max){  # Iterate through algorithm iter.max times
          previous_group_allocation = group_allocation
          update()
          iter <<- iter + 1  # Increment iteration count by 1
          if(identical(previous_group_allocation, group_allocation)){
            break  # End loop when iteration doesn't change the group allocation
          }
        }
        results()  # Get final variation calculations
      },
      
      # Updates the group assignments
      update = function() {
        number_variables = dim(x)[2] # Number of variables for each data point
        
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
        
        for (i in 1:(nrow(x))){  # Check each data point for it's closest cluster mean
          closest_group = NULL
          closest_distance = 10000000  # Arbitrary large value
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
        # Calculates within cluster and between cluster sum of squares
        # Total within group sum of squares
        tot.withinss <<- 0
        for(group_number in 1:k){
          for(i in groups[[group_number]]){
            tot.withinss <<- tot.withinss + as.numeric(t((x[i,] - unlist(cluster_means[group_number,])))%*%(x[i,] - unlist(cluster_means[group_number,])))
          }
        }
        betweenss <<- 0  # Between group sum of squares
        global_mean = (1/nrow(x))*colSums(x)
        for(group_number in 1:k){
          betweenss <<- betweenss + as.numeric(length(groups[[group_number]])%*%t(unlist(cluster_means[group_number,]) - global_mean)%*%(unlist(cluster_means[group_number,]) - global_mean))
        }
        tot.ss <<- betweenss + tot.withinss
      }
      )
  )
