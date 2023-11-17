

# Function to calculate entropy for a given dataset and attribute
entropy <- function(dataset, attribute) {
  class_column <- names(dataset)[ncol(dataset)]
  class_probabilities <- table(dataset[[class_column]]) / nrow(dataset)
  attribute_values <- unique(dataset[[attribute]])
  
  entropy_value <- -sum(sapply(attribute_values, function(value) {
    subset_data <- dataset %>% filter({{ attribute }} == value)
    class_prob <- table(subset_data[[class_column]]) / nrow(subset_data)
    entropy <- -sum(class_prob * log2(class_prob))
    entropy * nrow(subset_data) / nrow(dataset)
  }))
  
  return(entropy_value)
}







# Function to calculate Gini Index for a given dataset and attribute
gini_index <- function(dataset, attribute) {
  class_column <- names(dataset)[ncol(dataset)]
  attribute_values <- unique(dataset[[attribute]])
  
  gini_index_value <- sum(sapply(attribute_values, function(value) {
    subset_data <- dataset %>% filter({{ attribute }} == value)
    class_prob <- table(subset_data[[class_column]]) / nrow(subset_data)
    1 - sum(class_prob^2)
  }))
  
  return(gini_index_value)
}






# Function to calculate Gain Ratio for a given dataset and attribute
gain_ratio <- function(dataset, target_attribute, attribute) {
  info_gain <- entropy(dataset, target_attribute) - entropy(dataset, attribute)
  intrinsic_info <- calculate_info_attribute(dataset, attribute, unique(dataset[[attribute]]))
  
  if (intrinsic_info == 0) {
    return(0) # Avoid division by zero
  }
  
  return(info_gain / intrinsic_info)
}






# Function to choose the best attribute splitting technique and attribute
choose_attribute <- function(dataset, attribute_names, technique) {
  attribute_evaluations <- sapply(attribute_names, function(attribute) {
    switch(technique,
           "Information Gain" = entropy(dataset, attribute),
           "Gini Index" = gini_index(dataset, attribute),
           "Gain Ratio" = gain_ratio(dataset, attribute)
    )
  })
  
  best_attribute <- attribute_names[which.max(attribute_evaluations)]
  return(best_attribute)
}







# Function to calculate information for a given attribute in a dataset
calculate_info_attribute <- function(dataset, attribute, attribute_values) {
  info <- 0
  class_column <- names(dataset)[ncol(dataset)]
  total_instances <- nrow(dataset)
  
  for (value in attribute_values) {
    subset_data <- dataset %>% filter({{ attribute }} == value)
    class_prob <- table(subset_data[[class_column]]) / nrow(subset_data)
    info_value <- -sum(class_prob * log2(class_prob))
    info <- info + (nrow(subset_data) / total_instances) * info_value
  }
  
  return(info)
}





# Function to calculate information for the target attribute in a dataset
calculate_info <- function(target_attribute_values) {
  class_prob <- table(target_attribute_values) / length(target_attribute_values)
  info <- -sum(class_prob * log2(class_prob))
  return(info)
}







# TDIDT algorithm using different attribute splitting techniques for Decision Tree
tdidt <- function(dataset, target_attribute, attribute_names, splitting_technique, attribute = NULL) {
  # Check if all instances have the same class
  if (length(unique(dataset[[target_attribute]])) == 1) {
    return(list(class = unique(dataset[[target_attribute]])))
  }
  
  # Check if there are no more attributes to split on
  if (length(attribute_names) == 0) {
    return(list(class = "Majority Class"))
  }
  
  # Calculate the information for the target attribute
  target_info <- calculate_info(dataset[[target_attribute]])
  
  # Calculate the split metric for each attribute
  split_metric_values <- vector("numeric", length(attribute_names))
  for (i in seq_along(attribute_names)) {
    col <- attribute_names[i]
    col_values <- dataset[[col]]
    if (splitting_technique == "Information Gain") {
      split_metric_values[i] <- target_info - calculate_info_attribute(dataset, col, col_values)
    } else if (splitting_technique == "Gini Index") {
      split_metric_values[i] <- gini_index(dataset, col)
    } else if (splitting_technique == "Gain Ratio") {
      split_metric_values[i] <- gain_ratio(dataset, col)
    }
  }
  
  # Find the attribute with the maximum split metric value
  max_split_metric_value <- max(split_metric_values)
  max_split_metric_attribute <- attribute_names[which.max(split_metric_values)]
  
  # If we are in the recursive call, use the provided attribute
  if (!is.null(attribute)) {
    max_split_metric_attribute <- attribute
  }
  
  # Create a decision tree node with the attribute that has the maximum split metric value
  node <- list(attribute = max_split_metric_attribute, children = list())
  
  # Remove the selected attribute from the list and continue building the tree
  new_attribute_names <- attribute_names[attribute_names != max_split_metric_attribute]
  
  # Recursively build decision tree for each value of the selected attribute
  for (value in unique(dataset[[max_split_metric_attribute]])) {
    subset_data <- subset(dataset, dataset[[max_split_metric_attribute]] == value)
    if (nrow(subset_data) == 0) {
      # If no instances have this attribute value, use the majority class
      node$children[[as.character(value)]] <- list(class = "Majority Class")
    } else {
      node$children[[as.character(value)]] <- tdidt(subset_data, target_attribute, new_attribute_names, splitting_technique, attribute = NULL)
    }
  }
  
  return(node)
}


predict_decision_tree <- function(tree, data) {
  predictions <- character(nrow(data))
  
  for (i in 1:nrow(data)) {
    node <- tree
    while (is.list(node)) {
      attribute <- node$attribute
      attribute_value <- data[[attribute]][i]
      
      if (!is.null(node$children[[as.character(attribute_value)]])) {
        node <- node$children[[as.character(attribute_value)]]
      } else {
        # If the attribute value is not present in the tree, use the majority class of the training data
        majority_class <- names(table(data$target_col))[which.max(table(data$target_col))]
        predictions[i] <- majority_class
        break
      }
      
     
    }
    
    if (!is.null(node) && !is.list(node)) {
      predictions[i] <- node$class
    }
  }
  
  return(predictions)
}



# Function definitions and TDIDT algorithm

# Example usage:






# Split data into training (75%) and testing (25%) sets
set.seed(123)  # Set seed for reproducibility
train_indices <- createDataPartition(train$target_col, p = 0.75, list = FALSE)
train_data <- train[train_indices, ]
test_data <- train[-train_indices, ]



# Build the decision trees using the balanced training data
target_attribute <- "target_col"
attribute_names <- names(train_data)[1:(ncol(train_data) - 1)]

decision_tree_info_gain <- tdidt(train_data, target_attribute, attribute_names, "Information Gain")
decision_tree_gini_index <- tdidt(train_data, target_attribute, attribute_names, "Gini Index")
#decision_tree_gain_ratio <- tdidt(train_data_balanced, target_attribute, attribute_names, "Gain Ratio")

# Make predictions on the test data using the decision trees
test_predictions_info_gain <- predict_decision_tree(decision_tree_info_gain, test_data)
test_predictions_gini_index <- predict_decision_tree(decision_tree_gini_index, test_data)
#test_predictions_gain_ratio <- predict_decision_tree(decision_tree_gain_ratio, test_data)

# Calculate accuracy
accuracy_info_gain <- sum(test_predictions_info_gain == test_data$target_col) / nrow(test_data)
accuracy_gini_index <- sum(test_predictions_gini_index == test_data$target_col) / nrow(test_data)
#accuracy_gain_ratio <- sum(test_predictions_gain_ratio == test_data$target_col) / nrow(test_data)

cat("Accuracy with Information Gain:", accuracy_info_gain, "\n")
cat("Accuracy with Gini Index:", accuracy_gini_index, "\n")
#cat("Accuracy with Gain Ratio:", accuracy_gain_ratio, "\n")

# Convert target class to factor with all possible levels
all_levels <- unique(c(test_predictions_info_gain, test_predictions_gini_index, 
                       #test_predictions_gain_ratio, 
                       test_data$target_col))
test_data$target_col <- factor(test_data$target_col, levels = all_levels)
test_predictions_info_gain <- factor(test_predictions_info_gain, levels = all_levels)
test_predictions_gini_index <- factor(test_predictions_gini_index, levels = all_levels)
#test_predictions_gain_ratio <- factor(test_predictions_gain_ratio, levels = all_levels)

# Create confusion matrices using confusionMatrix() function
confusion_matrix_info_gain <- confusionMatrix(test_predictions_info_gain, test_data$target_col)
confusion_matrix_gini_index <- confusionMatrix(test_predictions_gini_index, test_data$target_col)
# confusion_matrix_gain_ratio <- confusionMatrix(test_predictions_gain_ratio, test_data$target_col)

print("Confusion Matrix with Information Gain:")
print(confusion_matrix_info_gain)

print("Confusion Matrix with Gini Index:")
print(confusion_matrix_gini_index)

# print("Confusion Matrix with Gain Ratio:")
# print(confusion_matrix_gain_ratio)
