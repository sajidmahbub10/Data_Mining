# Define a function to calculate entropy
calculate_entropy <- function(probs) {
  -sum(probs * log2(probs))
}

# Define a function to calculate Gini Index
calculate_gini <- function(probs) {
  1 - sum(probs^2)
}

# Define a function to calculate Gain Ratio
calculate_gain_ratio <- function(split_info, info_gain) {
  info_gain / split_info
}

# Define a function to select the best attribute based on a given method
select_best_attribute <- function(data, target_col, attributes, method) {
  target_levels <- levels(data[[target_col]])
  num_instances <- nrow(data)
  base_info <- calculate_entropy(table(data[[target_col]]) / num_instances)
  
  best_attribute <- NULL
  best_value <- NULL
  best_info_gain <- 0
  
  for (attr in attributes) {
    attr_values <- levels(data[[attr]])
    attr_info_gain <- 0
    
    for (value in attr_values) {
      subset <- data[data[[attr]] == value, ]
      num_subset <- nrow(subset)
      
      if (num_subset > 0) {
        subset_probs <- table(subset[[target_col]]) / num_subset
        subset_info <- if (method == "entropy") calculate_entropy(subset_probs)
        else if (method == "gini") calculate_gini(subset_probs)
        else 0
        
        attr_info_gain <- attr_info_gain + (num_subset / num_instances) * subset_info
      }
    }
    
    split_info <- -sum((table(data[[attr]]) / num_instances) * log2(table(data[[attr]]) / num_instances))
    info_gain <- base_info - attr_info_gain
    
    if (method == "gain_ratio") {
      gain_ratio <- calculate_gain_ratio(split_info, info_gain)
      info_gain <- gain_ratio
    }
    
    if (info_gain > best_info_gain) {
      best_info_gain <- info_gain
      best_attribute <- attr
    }
  }
  
  return(best_attribute)
}

# Define the TDIDT algorithm
tdidt <- function(data, target_col, attributes, method) {
  if (length(attributes) == 0 || length(levels(data[[target_col]])) == 1) {
    # Create a leaf node with the majority class label
    leaf_node <- list("leaf", table(data[[target_col]]) %>% which.max() %>% names())
    return(leaf_node)
  }
  
  best_attribute <- select_best_attribute(data, target_col, attributes, method)
  
  tree <- list()
  tree$attribute <- best_attribute
  tree$children <- list()
  
  attr_values <- levels(data[[best_attribute]])
  for (value in attr_values) {
    subset <- data[data[[best_attribute]] == value, ]
    if (nrow(subset) == 0) {
      # Create a leaf node with the majority class label
      leaf_node <- list("leaf", table(data[[target_col]]) %>% which.max() %>% names())
      tree$children[[as.character(value)]] <- leaf_node
    } else {
      subset_attributes <- attributes[attributes != best_attribute]
      subtree <- tdidt(subset, target_col, subset_attributes, method)
      tree$children[[as.character(value)]] <- subtree
    }
  }
  
  return(tree)
}


# Function to predict using the decision tree
predict_tree <- function(tree, instance) {
  if (tree[[1]] == "leaf") {
    return(tree[[2]])
  } else {
    attr <- tree$attribute
    attr_value <- instance[attr]
    subtree <- tree$children[[as.character(attr_value)]]
    if (is.null(subtree)) {
      # If the attribute value is not in the tree, return the majority class label
      return(table(instance[[target_col]]) %>% which.max() %>% names())
    }
    return(predict_tree(subtree, instance))
  }
}

data <- read.csv("D:/11th Semester/Data Warehousing and Mining/Mid/Assignment/traindata.csv")  # Replace with your actual file path


attributes <- c("age","job","marital","education","default","housing","loan","contact","month","day_of_week","duration","campaign","poutcome")
target_col <- "target_col"

library(ggplot2)

# Function to perform k-fold cross-validation and calculate confusion matrix
k_fold_cross_validation_confusion <- function(data, k, method) {
  fold_size <- nrow(data) %/% k
  confusion_matrices <- list()
  
  for (i in 1:k) {
    # Split data into training and testing sets
    test_indices <- ((i - 1) * fold_size + 1):(i * fold_size)
    test_data <- data[test_indices, ]
    train_data <- data[-test_indices, ]
    
    # Build decision tree using TDIDT algorithm
    tree <- tdidt(train_data, target_col, attributes, method)
    
    # Predict using the decision tree
    predicted <- character(0)
    actual <- character(0)
    for (j in 1:nrow(test_data)) {
      instance <- test_data[j, ]
      prediction <- predict_tree(tree, instance)
      predicted <- c(predicted, prediction)
      actual <- c(actual, instance[[target_col]])
    }
    
    # Calculate confusion matrix
    confusion <- table(Actual = actual, Predicted = predicted)
    confusion_matrices[[i]] <- confusion
  }
  
  return(confusion_matrices)
}

# Example usage:
confusion_matrices <- k_fold_cross_validation_confusion(data, k, "entropy")

# Function to visualize confusion matrix
visualize_confusion_matrix <- function(confusion) {
  ggplot(data.frame(confusion), aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = 0.5) +
    labs(x = "Actual", y = "Predicted", title = "Confusion Matrix") +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Visualize confusion matrix for each fold
for (i in 1:k) {
  print(visualize_confusion_matrix(confusion_matrices[[i]]))
}