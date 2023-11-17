View(data)
data <- train
# Function to calculate entropy
calculate_entropy <- function(data) {
  target_values <- table(data$target)
  probabilities <- target_values / sum(target_values)
  entropy <- -sum(probabilities * log2(probabilities))
  return(entropy)
}



# Function to calculate Gini Index of Diversity
calculate_gini <- function(data) {
  target_values <- table(data$target)
  probabilities <- target_values / sum(target_values)
  gini <- 1 - sum(probabilities^2)
  return(gini)
}

# Function to calculate Gain Ratio
calculate_gain_ratio <- function(data, attribute) {
  total_entropy <- calculate_entropy(data)
  
  attribute_values <- unique(data[[attribute]])
  total_instances <- nrow(data)
  
  weighted_entropy <- 0
  for (value in attribute_values) {
    subset_data <- subset(data, data[[attribute]] == value)
    weight <- nrow(subset_data) / total_instances
    weighted_entropy <- weighted_entropy + weight * calculate_entropy(subset_data)
  }
  
  gain_ratio <- (total_entropy - weighted_entropy) / (-sum(attribute_values / total_instances * log2(attribute_values / total_instances)))
  return(gain_ratio)
}

# Function to split data based on the selected attribute
split_data <- function(data, attribute, value) {
  return(subset(data, data[[attribute]] == value))
}

# Function to find the best attribute using the selected attribute selection algorithm
find_best_attribute <- function(data, attribute_selection_algorithm) {
  attributes <- names(data)[-ncol(data)]
  
  if (attribute_selection_algorithm == "entropy") {
    info_gains <- sapply(attributes, function(attribute) calculate_entropy(data) - calculate_entropy(split_data(data, attribute, data[[attribute]])))
  } else if (attribute_selection_algorithm == "gini") {
    info_gains <- sapply(attributes, function(attribute) calculate_gini(data) - calculate_gini(split_data(data, attribute, data[[attribute]])))
  } else if (attribute_selection_algorithm == "gain_ratio") {
    info_gains <- sapply(attributes, function(attribute) calculate_gain_ratio(data, attribute))
  }
  
  best_attribute <- names(info_gains)[which.max(info_gains)]
  return(best_attribute)
}





# Function to create a decision tree
create_decision_tree <- function(data, attribute_selection_algorithm, parent_attribute = NULL, parent_value = NULL) {
  target_values <- table(data$target)
  
  # Base case 1: If all instances have the same target value, return a leaf node with that value
  if (length(target_values) == 1) {
    return(names(target_values)[1])
  }
  
  # Base case 2: If there are no attributes left to split on, return a leaf node with the majority target value
  if (is.null(names(data)[-ncol(data)])) {
    majority_target <- names(target_values)[which.max(target_values)]
    return(majority_target)
  }
  
  # Base case 3: If the current data is a subset of the parent data, and the parent value is the same, return the parent value
  if (!is.null(parent_attribute) && identical(data, subset(parent_attribute, parent_attribute[[parent_attribute]] == parent_value))) {
    return(parent_value)
  }
  
  # Find the best attribute to split on
  best_attribute <- find_best_attribute(data, attribute_selection_algorithm)
  
  # Create a new decision tree node with the selected attribute
  tree <- list(attribute = best_attribute, children = list())
  
  # Recursively create subtrees for each value of the selected attribute
  attribute_values <- unique(data[[best_attribute]])
  for (value in attribute_values) {
    subset_data <- split_data(data, best_attribute, value)
    subtree <- create_decision_tree(subset_data, attribute_selection_algorithm, data, value)
    tree$children[[as.character(value)]] <- subtree
  }
  
  return(tree)
}









# Function to make predictions using the decision tree
predict_tree <- function(tree, instance) {
  if (is.character(tree)) {
    return(tree)
  }
  
  attribute <- tree$attribute
  value <- instance[[attribute]]
  subtree <- tree$children[[as.character(value)]]
  
  return(predict_tree(subtree, instance))
}

# Function to evaluate the decision tree using 5-fold cross-validation
evaluate_decision_tree <- function(data, attribute_selection_algorithm) {
  set.seed(123)
  folds <- cut(seq(1, nrow(data)), breaks = 5, labels = FALSE)
  accuracy <- numeric(5)
  
  for (i in 1:5) {
    test_indices <- which(folds == i, arr.ind = TRUE)
    test_data <- data[test_indices, ]
    train_data <- data[-test_indices, ]
    
    tree <- create_decision_tree(train_data, attribute_selection_algorithm)
    
    predicted <- apply(test_data, 1, predict_tree, tree = tree)
    actual <- test_data$target
    accuracy[i] <- sum(predicted == actual) / length(actual)
  }
  
  return(mean(accuracy))
}

# Assuming you have the dataset loaded in the 'data' variable
# Replace 'attribute_selection_algorithm' with "entropy", "gini", or "gain_ratio"
accuracy <- evaluate_decision_tree(data, "gini")
cat("Accuracy using Gini Index:", accuracy, "\n")

