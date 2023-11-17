# Function to calculate entropy
calculate_entropy <- function(target_values) {
  class_probs <- table(target_values) / length(target_values)
  entropy <- -sum(class_probs * log2(class_probs))
  return(entropy)
}

# Function to calculate Gini Index for a target attribute
calculate_gini_index <- function(target) {
  proportions <- table(target) / length(target)
  gini_index <- 1 - sum(proportions^2)
  return(gini_index)
}

# Function to calculate Gain Ratio with handling for missing values
calculate_gain_ratio <- function(data, attribute, target) {
  total_entropy <- calculate_entropy(data[[target]])
  attribute_values <- unique(data[[attribute]])
  split_entropy <- 0
  total_instances <- nrow(data)
  
  for (value in attribute_values) {
    subset_data <- data[data[[attribute]] == value, ]
    subset_entropy <- calculate_entropy(subset_data[[target]])
    split_entropy <- split_entropy + (nrow(subset_data) / total_instances) * subset_entropy
  }
  
  gain_ratio <- (total_entropy - split_entropy) / calculate_entropy(data[[attribute]])
  
  # Handle NaN (resulting from division by 0 or log(0)) and replace with 0
  if (is.nan(gain_ratio)) {
    gain_ratio <- 0
  }
  
  return(gain_ratio)
}

# Function to implement TDIDT algorithm with handling for empty datasets and attributes
tdidt <- function(data, target, attributes, selection_algorithm) {
  unique_classes <- unique(data[[target]])
  if (length(unique_classes) == 1) {
    return(unique_classes)
  }
  
  if (length(attributes) == 0) {
    majority_class <- names(sort(table(data[[target]]), decreasing = TRUE)[1])
    return(majority_class)
  }
  
  selected_attribute <- NULL
  max_value <- -Inf
  
  for (attribute in attributes) {
    if (selection_algorithm == "entropy") {
      value <- calculate_gain_ratio(data, attribute, target)
    } else if (selection_algorithm == "gini") {
      value <- calculate_gini_index(data[[attribute]])
    } else if (selection_algorithm == "gain_ratio") {
      value <- calculate_gain_ratio(data, attribute, target)
    }
    
    if (value > max_value) {
      max_value <- value
      selected_attribute <- attribute
    }
  }
  
  if (is.null(selected_attribute)) {
    majority_class <- names(sort(table(data[[target]]), decreasing = TRUE)[1])
    return(majority_class)
  }
  
  tree <- list()
  tree$attribute <- selected_attribute
  tree$children <- list()
  
  attribute_values <- unique(data[[selected_attribute]])
  
  for (value in attribute_values) {
    subset_data <- data[data[[selected_attribute]] == value, ]
    
    if (nrow(subset_data) == 0) {
      majority_class <- names(sort(table(data[[target]]), decreasing = TRUE)[1])
      tree$children[[as.character(value)]] <- majority_class
    } else {
      remaining_attributes <- attributes[!(attributes %in% selected_attribute)]
      tree$children[[as.character(value)]] <- tdidt(subset_data, target, remaining_attributes, selection_algorithm)
    }
  }
  
  return(tree)
}







# Function to count the number of branches and rules in the decision tree
count_branches_rules <- function(tree) {
  if (length(tree$children) == 0) {
    # A leaf node, return 1 branch and 0 rule
    return(list(branches = 1, rules = 0))
  }
  
  num_branches <- 1  # Initialize with 1 for the current branch (root)
  num_rules <- 0
  
  for (child in tree$children) {
    if (is.list(child)) {
      # Recursively count branches and rules for each child
      child_counts <- count_branches_rules(child)
      num_branches <- num_branches + child_counts$branches
      num_rules <- num_rules + child_counts$rules
    } else {
      # Reached a leaf node, add 1 rule
      num_rules <- num_rules + 1
    }
  }
  
  return(list(branches = num_branches, rules = num_rules))
}



# Create the decision tree using the Entropy attribute selection algorithm
attributes <- names(data)[names(data) != "target"]
tree_entropy <- tdidt(data, "target", attributes, "entropy")

# Create the decision tree using the Gini Index of Diversity attribute selection algorithm
tree_gini <- tdidt(data, "target", attributes, "gini")

# Create the decision tree using the Gain Ratio attribute selection algorithm
tree_gain_ratio <- tdidt(data, "target", attributes, "gain_ratio")

# Calculate the number of branches and rules for each decision tree
branches_rules_entropy <- count_branches_rules(tree_entropy)
branches_rules_gini <- count_branches_rules(tree_gini)
branches_rules_gain_ratio <- count_branches_rules(tree_gain_ratio)




# Function to perform 5-fold cross-validation and get the confusion matrix
evaluate_tree <- function(data, target, attributes, selection_algorithm, fold) {
  train_data <- data[-fold, ]
  test_data <- data[fold, ]
  
  tree <- tdidt(train_data, target, attributes, selection_algorithm)
  
  # Predict for all test instances at once
  predictions <- apply(test_data, 1, function(instance) {
    prediction <- predict_instance(tree, instance, target)
    return(prediction)
  })
  
  # Convert predictions and test_data[[target]] to factors with the same levels
  predictions <- factor(predictions, levels = levels(data[[target]]))
  test_labels <- factor(test_data[[target]], levels = levels(data[[target]]))
  
  # Create the confusion matrix
  confusion <- table(predictions, test_labels)
  return(confusion)
}

# Function to predict the target class for a single instance
predict_instance <- function(tree, instance, target) {
  if (length(tree$children) == 0) {
    return(tree)
  }
  
  attribute_value <- instance[tree$attribute]
  
  if (is.null(tree$children[[as.character(attribute_value)]])) {
    return(names(sort(table(target), decreasing = TRUE)[1]))
  } else {
    return(predict_instance(tree$children[[as.character(attribute_value)]], instance, target))
  }
}

# Function to calculate the average accuracy from the confusion matrix
calculate_accuracy <- function(confusion_matrix) {
  confusion_matrix <- as.factor(confusion_matrix)
  return(mean(diag(confusion_matrix)))
}

# ... Rest of the code remains unchanged ...





# Perform 5-fold cross-validation for each tree
num_folds <- 5
folds <- split(1:nrow(data), 1:nrow(data) %% num_folds)

confusion_entropy <- matrix(0, nrow = length(unique(data$target)), ncol = length(unique(data$target)))
confusion_gini <- matrix(0, nrow = length(unique(data$target)), ncol = length(unique(data$target)))
confusion_gain_ratio <- matrix(0, nrow = length(unique(data$target)), ncol = length(unique(data$target)))

for (fold in folds) {
  # Convert fold indices to integers
  fold <- as.integer(fold)
  
  # Evaluate decision tree using Entropy
  confusion_entropy_fold <- evaluate_tree(data, "target", attributes, "entropy", fold)
  confusion_entropy <- confusion_entropy + confusion_entropy_fold
  
  # Evaluate decision tree using Gini Index
  confusion_gini_fold <- evaluate_tree(data, "target", attributes, "gini", fold)
  confusion_gini <- confusion_gini + confusion_gini_fold
  
  # Evaluate decision tree using Gain Ratio
  confusion_gain_ratio_fold <- evaluate_tree(data, "target", attributes, "gain_ratio", fold)
  confusion_gain_ratio <- confusion_gain_ratio + confusion_gain_ratio_fold
}

# Function to calculate the average accuracy from the confusion matrix
calculate_accuracy <- function(confusion_matrix) {
  sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

# Calculate average accuracies for each attribute selection algorithm
accuracy_entropy <- calculate_accuracy(confusion_entropy)
accuracy_gini <- calculate_accuracy(confusion_gini)
accuracy_gain_ratio <- calculate_accuracy(confusion_gain_ratio)

# Print average accuracies
cat("Average Accuracy (Entropy):", round(accuracy_entropy, 4), "\n")
cat("Average Accuracy (Gini):", round(accuracy_gini, 4), "\n")
cat("Average Accuracy (Gain Ratio):", round(accuracy_gain_ratio, 4), "\n")

# Visualize the confusion matrix for each attribute selection algorithm
print("Confusion Matrix (Entropy):")
print(confusion_entropy)

print("Confusion Matrix (Gini):")
print(confusion_gini)

print("Confusion Matrix (Gain Ratio):")
print(confusion_gain_ratio)
