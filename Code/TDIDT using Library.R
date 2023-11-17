# Load required libraries
library(dplyr)
library(caret)
library(rpart)

# Calculate accuracy from confusion matrix
calculate_accuracy <- function(confusion_matrix) {
  sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

# Count number of branches and rules in a decision tree
count_branches_rules <- function(decision_tree) {
  num_branches <- sum(attr(decision_tree, "splitval") != -1)
  num_rules <- sum(attr(decision_tree, "splitval") == -1)
  return(list(branches = num_branches, rules = num_rules))
}

# Example usage:
data <- read.csv("D:/11th Semester/Data Warehousing and Mining/Mid/Assignment/traindata.csv")  # Replace with your actual file path

# Convert the target variable to a binary numeric representation (0 and 1)
data$target_col <- as.factor(data$target_col)

# Split data into training (70%) and testing (30%) sets
set.seed(123)  # Set seed for reproducibility
train_indices <- createDataPartition(data$target_col, p = 0.60, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Build the decision trees using the training data
target_attribute <- "target_col"
attribute_names <- names(train_data)[1:(ncol(train_data) - 1)]

# Make predictions on the test data using the decision trees
decision_tree_info_gain <- rpart(as.formula(paste(target_attribute, "~", paste(attribute_names, collapse = "+"))),
                                 data = train_data, method = "class", , parms = list(split = "information"))

decision_tree_gini_index <- rpart(as.formula(paste(target_attribute, "~", paste(attribute_names, collapse = "+"))),
                                  data = train_data, method = "class", parms = list(split = "gini"))

decision_tree_gain_ratio <- rpart(as.formula(paste(target_attribute, "~", paste(attribute_names, collapse = "+"))),
                                  data = train_data, method = "class", parms = list(split = "ratio"))

# Count branches and rules for each decision tree model
count_info_gain <- count_branches_rules(decision_tree_info_gain)
count_gini_index <- count_branches_rules(decision_tree_gini_index)
count_gain_ratio <- count_branches_rules(decision_tree_gain_ratio)

# Make predictions on the test data using the decision trees
test_predictions_info_gain <- predict(decision_tree_info_gain, newdata = test_data, type = "class")
test_predictions_gini_index <- predict(decision_tree_gini_index, newdata = test_data, type = "class")
test_predictions_gain_ratio <- predict(decision_tree_gain_ratio, newdata = test_data, type = "class")

# Convert the target variable in test_data to a factor with levels "0" and "1"
test_data$target_col <- as.factor(test_data$target_col)

# Now you can create confusion matrices
confusion_matrix_info_gain <- confusionMatrix(test_predictions_info_gain, test_data$target_col)$table
confusion_matrix_gini_index <- confusionMatrix(test_predictions_gini_index, test_data$target_col)$table
confusion_matrix_gain_ratio <- confusionMatrix(test_predictions_gain_ratio, test_data$target_col)$table

# Calculate accuracy for each decision tree model
accuracy_info_gain <- calculate_accuracy(confusion_matrix_info_gain)
accuracy_gini_index <- calculate_accuracy(confusion_matrix_gini_index)
accuracy_gain_ratio <- calculate_accuracy(confusion_matrix_gain_ratio)

# Print accuracy for each model
cat("Accuracy with Information Gain:", accuracy_info_gain, "\n")
cat("Accuracy with Gini Index:", accuracy_gini_index, "\n")
cat("Accuracy with Gain Ratio:", accuracy_gain_ratio, "\n")

# Print number of branches and rules for each model
cat("Number of branches with Information Gain:", count_info_gain$branches, "\n")
cat("Number of rules with Information Gain:", count_info_gain$rules, "\n")

cat("Number of branches with Gini Index:", count_gini_index$branches, "\n")
cat("Number of rules with Gini Index:", count_gini_index$rules, "\n")

cat("Number of branches with Gain Ratio:", count_gain_ratio$branches, "\n")
cat("Number of rules with Gain Ratio:", count_gain_ratio$rules, "\n")

print("Confusion Matrix with Information Gain:")
print(confusion_matrix_info_gain)

print("Confusion Matrix with Gini Index:")
print(confusion_matrix_gini_index)

print("Confusion Matrix with Gain Ratio:")
print(confusion_matrix_gain_ratio)
