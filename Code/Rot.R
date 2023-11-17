# Function to calculate Euclidean distance between two points
euclidean_distance <- function(a1, a2, b1, b2) {
  sqrt((a1 - b1)^2 + (a2 - b2)^2)
}

# Function to calculate Manhattan distance between two points
manhattan_distance <- function(a1, a2, b1, b2) {
  abs(a1 - b1) + abs(a2 - b2)
}

# Function to calculate maximum dimensional distance between two points
max_dimensional_distance <- function(a1, a2, b1, b2) {
  max(abs(a1 - b1), abs(a2 - b2))
}





# Function to perform k-NN classification
knn_classification <- function(test_data, train_data, k, distance_function) {
  predictions <- character(nrow(test_data))
  
  for (i in 1:nrow(test_data)) {
    distances <- numeric(nrow(train_data))
    
    for (j in 1:nrow(train_data)) {
      distances[j] <- distance_function(test_data$age[i], test_data$job[i],
                                        train_data$age[j], train_data$job[j])
    }
    
    k_indices <- order(distances)[1:k]
    k_class <- train_data$Class[k_indices]
    predictions[i] <- names(which.max(table(k_class)))
  }
  
  return(predictions)
}

# Define the test data and train data

# Perform k-NN classification using different distance measures and values of k
k_values <- c(1,3,5,7,9,11,13,15,17,19)
distance_functions <- list(euclidean_distance, manhattan_distance, max_dimensional_distance)
Names <- list("Euclidean", "Manhattan","Max_dimensional_distance")

for (k in k_values) {
  for (distance_function in distance_functions) {
    predictions <- knn_classification(test, train, k, distance_function)
    accuracy <- sum(predictions == test$Class) / nrow(test)
    recall <- sum(predictions == "yes" & test$Class == "yes") /
      sum(test$Class == "yes")
    

    cat("k:", k, "\n")
    cat("Accuracy:", accuracy * 100, "%\n")
    cat("Recall:", recall * 100, "%\n")
    cat("----------------------------\n")
  }
}


