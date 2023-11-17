euclidean_distance <- function(point1, point2) {
  sum((point1 - point2)^2)^0.5
}

manhattan_distance <- function(point1, point2) {
  sum(abs(point1 - point2))
}


max_dimension_distance <- function(point1, point2) {
  max(abs(point1 - point2))
}



KNN <- function(train, test, distance_metric, k) {
  # Calculate distances between each test point and train point
  distances <- matrix(NA, nrow = nrow(test), ncol = nrow(train))
  for (i in 1:nrow(train)) {
    
    for (j in 1:nrow(test)) {
      
      if (distance_metric == "euclidean") {
        distances[i, j] <- euclidean_distance(test[i, ], train[j, ])
      } else if (distance_metric == "manhattan") {
        distances[i, j] <- manhattan_distance(test[i, ], train[j, ])
      } else if (distance_metric == "maximum_dimension") {
        distances[i, j] <- max_dimension_distance(test[i, ], train[j, ])
      }
    }
  }
  
  # Find the k nearest neighbors for each test point
  neighbors <- matrix(NA, nrow = nrow(test), ncol = k)
  for (i in 1:nrow(test)) {
    sorted_distances <- sort(distances[i, ])
    neighbors[i, ] <- order(distances[i, ]) %>% head(k)
  }
  
  # Perform majority voting to predict the labels
  predictions <- vector("factor", length = nrow(test))
  for (i in 1:nrow(test)) {
    neighbor_labels <- train[neighbors[i, ], ]$y
    predictions[i] <- names(which.max(table(neighbor_labels)))
  }
  
  # Compute accuracy
  correct_predictions <- sum(predictions == test$yl)
  accuracy <- correct_predictions / nrow(test)
  
  return(accuracy)
}



k <- 3
distance_metric <- "euclidean"
accuracy <- KNN(train_data, test_data, distance_metric, k)
cat("Accuracy:", accuracy, "\n")

library(class)
View(knn)
