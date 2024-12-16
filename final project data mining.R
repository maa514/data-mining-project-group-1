# Installing packages
install.packages("readr")
library(readr)
install.packages("corrplot")
library(corrplot)
library(ggplot2)

# Reading the data set into R
property_sales_data <- read_csv("2002-2018-property-sales-data.csv")

# Check for missing values
colSums(is.na(property_sales_data))

# Remove rows where Sale_price is zero
property_sales_data <- property_sales_data[property_sales_data$Sale_price > 0, ]


## basic data visualizations
# Create histogram with adjusted x-axis limits
property_sales_data[is.na(property_sales_data$Sale_price) | !is.finite(property_sales_data$Sale_price), ]
property_sales_data <- property_sales_data[!is.na(property_sales_data$Sale_price) & is.finite(property_sales_data$Sale_price), ]
filtered_data <- property_sales_data[property_sales_data$Sale_price >= 0 & property_sales_data$Sale_price <= 500000, ]
ggplot(filtered_data, aes(x = Sale_price)) +
  geom_histogram(binwidth = 5000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Housing Prices", x = "Sale Price", y = "Count") +
  theme_minimal()


# Summary statistics for Sale_price to see outliers
summary(property_sales_data$Sale_price)

# Apply log transformation to Sale_price to better visualize
property_sales_data$LogSale_price <- log(property_sales_data$Sale_price)
# Create a histogram of the log-transformed Sale_price
ggplot(property_sales_data, aes(x = LogSale_price)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Log-Transformed Distribution of Housing Prices", x = "Log of Sale Price", y = "Count") +
  theme_minimal()



### DATA PREPROCESSING

## Feature selection:
# Calculate the correlation matrix
numeric_data <- property_sales_data[, sapply(property_sales_data, is.numeric)]
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Plot the correlation matrix with values
corrplot(correlation_matrix, 
         method = "color",                    # Color-coded correlations
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         type = "upper",                      # Display upper triangle only
         addCoef.col = "black",               # Add correlation values in black
         tl.col = "black",                    # Label color
         tl.srt = 45,                         # Rotate labels for readability
         number.cex = 0.7,                    # Size of correlation values
         title = "Correlation Matrix", 
         mar = c(0, 0, 2, 0))                 # Margins for the plot

# Because none of the correlation values are too high, we don't have to remove
# any columns from our feature selection analysis

## Normalization
numeric_data <- property_sales_data[, sapply(property_sales_data, is.numeric)]
install.packages("caret")
install.packages("lattice")
library(caret)
preProc <- preProcess(numeric_data, method = c("center", "scale"))
data_normalized <- predict(preProc, numeric_data)

summary(numeric_data)          # Original data
summary(data_normalized)       # Normalized data

# data_normalized can be used for PCA





# Performing PCA
# Perform PCA on normalized data
pca_result <- prcomp(data_normalized, center = TRUE, scale. = TRUE)

# View explained variance for each principal component
summary(pca_result)

# Visualizing Variance Explained
# Calculate proportion of variance explained by each PC
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_var <- cumsum(var_explained)

# Scree plot for variance explained
install.packages("ggplot2")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Create a data frame for variance explained and cumulative variance
df <- data.frame(
  PC = 1:length(var_explained),
  Variance = var_explained,
  Cumulative = cumulative_var
)

# Scree plot for variance explained
ggplot(df, aes(x = PC)) +
  # Bar plot for variance explained
  geom_bar(aes(y = Variance), stat = "identity", fill = "skyblue", alpha = 0.7) +
  # Line plot for cumulative variance
  geom_line(aes(y = Cumulative), color = "blue", size = 1) +
  # Points for cumulative variance
  geom_point(aes(y = Cumulative), color = "blue", size = 2) +
  # Add titles and labels
  labs(
    title = "Variance Explained by Principal Components",
    x = "Principal Components",
    y = "Proportion of Variance"
  ) +
  # Minimal theme for better readability
  theme_minimal() +
  # Optional: Adjust x and y scales if necessary
  scale_x_continuous(breaks = 1:length(var_explained))


# Selecting Top Principal Components
# Retain the top 8 principal components (covering ~82.27% of the variance)
reduced_data <- pca_result$x[, 1:8]  # First 8 PCs

# The reduced_data can now be used for further analysis (e.g., clustering, modeling)

## Models for categorized sale price
## Decision Tree
# Load necessary libraries
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("caret")

library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)

# Read the dataset
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Data preprocessing
# Remove rows with missing or zero Sale_price
property_sales_data <- property_sales_data %>% 
  filter(!is.na(Sale_price) & Sale_price > 0)

# Create a categorical variable for Sale_price (e.g., Low, Medium, High)
quantiles <- quantile(property_sales_data$Sale_price, probs = c(0.33, 0.66))
property_sales_data$Price_Category <- cut(
  property_sales_data$Sale_price,
  breaks = c(-Inf, quantiles, Inf),
  labels = c("Low", "Medium", "High")
)

# Select relevant columns for the model
# Removing non-predictive columns like Taxkey and Address
model_data <- property_sales_data %>%
  select(Price_Category, Year_Built, Fin_sqft, Lotsize, Bdrms, Fbath, Hbath)

# Remove rows with missing values in selected columns
model_data <- na.omit(model_data)

# Normalize numeric features
preProc <- preProcess(model_data[, -1], method = c("center", "scale"))
model_data_normalized <- cbind(
  Price_Category = model_data$Price_Category,
  predict(preProc, model_data[, -1])
)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(model_data_normalized), 0.7 * nrow(model_data_normalized))
train_data <- model_data_normalized[train_index, ]
test_data <- model_data_normalized[-train_index, ]

# Build the decision tree
decision_tree <- rpart(
  Price_Category ~ ., 
  data = train_data, 
  method = "class",
  control = rpart.control(cp = 0.01, minsplit = 20)
)

# Visualize the decision tree
rpart.plot(
  decision_tree, 
  type = 3, 
  extra = 102, 
  under = TRUE, 
  fallen.leaves = TRUE,
  tweak = 1.2,
  main = "Decision Tree for Property Price Category"
)

# Evaluate the model on the test data
predicted <- predict(decision_tree, test_data, type = "class")
confusion_matrix <- table(test_data$Price_Category, predicted)

# Print confusion matrix and accuracy
print("Confusion Matrix:")
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

# Print variable importance
print("Variable Importance:")
print(decision_tree$variable.importance)

## KNN algorithm
# Install necessary packages
necessary_packages <- c("rpart", "rpart.plot", "dplyr", "caret", "class", "ggplot2")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}
lapply(necessary_packages, install_if_missing)

# Load libraries
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(class)
library(ggplot2)

# Read the dataset
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Data preprocessing
# Remove rows with missing or zero Sale_price
property_sales_data <- property_sales_data %>% 
  filter(!is.na(Sale_price) & Sale_price > 0)

# Create a categorical variable for Sale_price (e.g., Low, Medium, High)
quantiles <- quantile(property_sales_data$Sale_price, probs = c(0.33, 0.66))
property_sales_data$Price_Category <- cut(
  property_sales_data$Sale_price,
  breaks = c(-Inf, quantiles, Inf),
  labels = c("Low", "Medium", "High")
)

# Select relevant columns for the model
model_data <- property_sales_data %>%
  select(Price_Category, Year_Built, Fin_sqft, Lotsize, Bdrms, Fbath, Hbath)

# Remove rows with missing values
model_data <- na.omit(model_data)

# Split data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(model_data), 0.7 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Prepare data for KNN
train_labels <- train_data$Price_Category
test_labels <- test_data$Price_Category
train_features <- train_data[, -1]
test_features <- test_data[, -1]

# Perform KNN with k = 5
k <- 5
knn_predictions <- knn(
  train = train_features,
  test = test_features,
  cl = train_labels,
  k = k
)

# Add predictions to the test data
test_data$Predicted <- knn_predictions

# Evaluate the KNN model
confusion_matrix <- table(test_data$Price_Category, test_data$Predicted)
knn_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print evaluation results
print("KNN Confusion Matrix:")
print(confusion_matrix)
print(paste("KNN Accuracy:", round(knn_accuracy, 4)))

# Visualize the results
# 1. Bar plot of prediction distribution
ggplot(test_data, aes(x = Predicted, fill = Predicted)) +
  geom_bar() +
  labs(title = "KNN Prediction Distribution", x = "Predicted Category", y = "Count") +
  theme_minimal()

# 2. Scatter plot of predictions
ggplot(test_data, aes(x = Fin_sqft, y = Lotsize, color = Predicted)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "KNN Predicted Results", x = "Finished Square Footage", y = "Lot Size") +
  theme_minimal()

# 3. Classification boundaries (example with two features: Year_Built and Fin_sqft)
# Create a grid of values for Year_Built and Fin_sqft
grid <- expand.grid(
  Year_Built = seq(min(test_data$Year_Built), max(test_data$Year_Built), length.out = 100),
  Fin_sqft = seq(min(test_data$Fin_sqft), max(test_data$Fin_sqft), length.out = 100)
)

# Ensure grid_matrix aligns with train_features columns
grid_matrix <- as.matrix(grid[, c("Year_Built", "Fin_sqft")])

# Predict on the grid
grid$Prediction <- knn(
  train = train_features[, c("Year_Built", "Fin_sqft")],
  test = grid_matrix,
  cl = train_labels,
  k = k
)

# Debugging output to validate Prediction
print("Grid with Predictions:")
print(head(grid))

# Fix Plot: Separate the data sources for `grid` and `test_data`
ggplot() +
  geom_tile(data = grid, aes(x = Year_Built, y = Fin_sqft, fill = Prediction), alpha = 0.3) +
  geom_point(data = test_data, aes(x = Year_Built, y = Fin_sqft, color = Predicted), size = 3) +
  labs(title = "KNN Classification Boundaries", x = "Year Built", y = "Finished Square Footage") +
  theme_minimal()


## SVM Model
# Install necessary packages (if not already installed)
necessary_packages <- c("e1071", "caret", "ggplot2", "dplyr")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}
lapply(necessary_packages, install_if_missing)

# Load libraries
library(e1071)
library(caret)
library(ggplot2)
library(dplyr)

# Load dataset
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Preprocess data
# Remove rows with missing or zero Sale_price
property_sales_data <- property_sales_data %>%
  filter(!is.na(Sale_price) & Sale_price > 0)

# Create a categorical variable for Sale_price (Low, Medium, High)
quantiles <- quantile(property_sales_data$Sale_price, probs = c(0.33, 0.66))
property_sales_data$Price_Category <- cut(
  property_sales_data$Sale_price,
  breaks = c(-Inf, quantiles, Inf),
  labels = c("Low", "Medium", "High")
)

# Select relevant columns for the model
model_data <- property_sales_data %>%
  select(Price_Category, Year_Built, Fin_sqft, Lotsize, Bdrms, Fbath, Hbath)

# Remove rows with missing values
model_data <- na.omit(model_data)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(model_data), 0.7 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# SVM Hyperparameter Tuning
svm_tune <- tune.svm(
  Price_Category ~ ., 
  data = train_data,
  kernel = "radial",
  cost = 10^(-1:2),      # Cost values: 0.1, 1, 10, 100
  gamma = c(0.1, 0.5, 1) # Gamma values: 0.1, 0.5, 1
)

# Retrieve the best SVM model
best_svm_model <- svm_tune$best.model

# Evaluate the best SVM model on test data
svm_predictions <- predict(best_svm_model, test_data)
svm_confusion_matrix <- table(test_data$Price_Category, svm_predictions)

# Print confusion matrix and accuracy
print("SVM Confusion Matrix:")
print(svm_confusion_matrix)
svm_accuracy <- sum(diag(svm_confusion_matrix)) / sum(svm_confusion_matrix)
print(paste("SVM Accuracy:", round(svm_accuracy, 4)))

# Visualizing tuning results
accuracy_results <- data.frame(
  Cost = svm_tune$performances$cost,
  Gamma = svm_tune$performances$gamma,
  Accuracy = 1 - svm_tune$performances$error
)

# Heatmap of tuning results
ggplot(accuracy_results, aes(x = factor(Cost), y = factor(Gamma), fill = Accuracy)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "SVM Accuracy by Cost and Gamma",
    x = "Cost",
    y = "Gamma"
  ) +
  theme_minimal()

# Confusion matrix heatmap
confusion_matrix_df <- as.data.frame(as.table(svm_confusion_matrix))
colnames(confusion_matrix_df) <- c("Actual", "Predicted", "Frequency")

ggplot(confusion_matrix_df, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = "SVM Confusion Matrix Heatmap",
    x = "Actual Category",
    y = "Predicted Category"
  ) +
  theme_minimal()


## Rule-based classifier
# Install necessary packages
necessary_packages <- c("rpart", "rpart.plot", "dplyr", "caret", "class", "ggplot2", "e1071")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}
lapply(necessary_packages, install_if_missing)

# Load libraries
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(class)
library(ggplot2)
library(e1071)

# Load the dataset
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Data preprocessing
# Remove rows with missing or zero Sale_price
property_sales_data <- property_sales_data %>%
  filter(!is.na(Sale_price) & Sale_price > 0)

# Create a categorical variable for Sale_price (Low, Medium, High)
quantiles <- quantile(property_sales_data$Sale_price, probs = c(0.33, 0.66))
property_sales_data$Price_Category <- cut(
  property_sales_data$Sale_price,
  breaks = c(-Inf, quantiles, Inf),
  labels = c("Low", "Medium", "High")
)

# Select relevant columns for the model
model_data <- property_sales_data %>%
  select(Price_Category, Year_Built, Fin_sqft, Lotsize, Bdrms, Fbath, Hbath)

# Remove rows with missing values in selected columns
model_data <- na.omit(model_data)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(model_data), 0.7 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Define the rule-based classifier
rule_based_classifier <- function(row) {
  # Enhanced rules for better classification
  if (row$Fin_sqft > 3000 & row$Lotsize > 8000 & row$Bdrms > 4) {
    return("High")
  } else if (row$Fin_sqft > 2000 & row$Lotsize > 5000 & row$Bdrms >= 3) {
    return("Medium")
  } else {
    return("Low")
  }
}

# Apply the rule-based classifier to the test data
test_data$Predicted <- apply(test_data, 1, function(row) rule_based_classifier(as.list(row)))

# Evaluate the rule-based classifier
confusion_matrix <- table(test_data$Price_Category, test_data$Predicted)

# Print confusion matrix and accuracy
print("Rule-Based Classifier Confusion Matrix:")
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Rule-Based Classifier Accuracy:", round(accuracy, 4)))

# Visualize the confusion matrix
# Convert confusion matrix to a data frame for ggplot
confusion_matrix_df <- as.data.frame(as.table(confusion_matrix))
colnames(confusion_matrix_df) <- c("Actual", "Predicted", "Frequency")

# Plot the confusion matrix heatmap with enhanced visualization
ggplot(confusion_matrix_df, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +  # Add labels to cells
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Rule-Based Classifier Confusion Matrix",
    x = "Actual Category",
    y = "Predicted Category",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12)
  )

# -------------------------------------------
# PREDICTION ON NEW DATA
# -------------------------------------------
# Example of unseen data (new observations)
new_data <- data.frame(
  Year_Built = c(2000, 1980, 2010),
  Fin_sqft = c(3500, 1800, 2500),
  Lotsize = c(9000, 4000, 6000),
  Bdrms = c(5, 2, 3),
  Fbath = c(3, 1, 2),
  Hbath = c(1, 1, 1)
)

# Predict using the rule-based classifier
new_data$Predicted <- apply(new_data, 1, function(row) rule_based_classifier(as.list(row)))

# Print predictions
print("Predictions for New Data:")
print(new_data)

# -------------------------------------------
# VISUALIZE PREDICTION RESULTS
# -------------------------------------------
# Plot the prediction results for new data
ggplot(new_data, aes(x = Fin_sqft, y = Lotsize, color = Predicted)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(
    title = "Predicted Results for New Data",
    x = "Finished Square Footage",
    y = "Lot Size",
    color = "Predicted Category"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12)
  )

## Bagging
# Install necessary packages
necessary_packages <- c("rpart", "rpart.plot", "dplyr", "caret", "class", "ggplot2", "e1071", "ipred")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}
lapply(necessary_packages, install_if_missing)

# Load libraries
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(class)
library(ggplot2)
library(e1071)
library(ipred)

# Read the dataset
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Data preprocessing
# Remove rows with missing or zero Sale_price
property_sales_data <- property_sales_data %>% 
  filter(!is.na(Sale_price) & Sale_price > 0)

# Create a categorical variable for Sale_price (Low, Medium, High)
quantiles <- quantile(property_sales_data$Sale_price, probs = c(0.33, 0.66))
property_sales_data$Price_Category <- cut(
  property_sales_data$Sale_price,
  breaks = c(-Inf, quantiles, Inf),
  labels = c("Low", "Medium", "High")
)

# Select relevant columns for the model
model_data <- property_sales_data %>%
  select(Price_Category, Year_Built, Fin_sqft, Lotsize, Bdrms, Fbath, Hbath)

# Remove rows with missing values in selected columns
model_data <- na.omit(model_data)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(model_data), 0.7 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Build a bagging model
bagging_model <- bagging(
  Price_Category ~ ., 
  data = train_data, 
  coob = TRUE  # Use out-of-bag error for validation
)

# Print the bagging model summary
print(bagging_model)

# Make predictions using the bagging model
bagging_predictions <- predict(bagging_model, test_data)

# Add predictions to the test_data
test_data$Predicted_Category <- bagging_predictions

# Evaluate the bagging model
bagging_confusion_matrix <- table(test_data$Price_Category, test_data$Predicted_Category)

# Print confusion matrix and accuracy
print("Bagging Classifier Confusion Matrix:")
print(bagging_confusion_matrix)
bagging_accuracy <- sum(diag(bagging_confusion_matrix)) / sum(bagging_confusion_matrix)
print(paste("Bagging Classifier Accuracy:", round(bagging_accuracy, 4)))

# View a sample of the test data with predictions
print("Sample of test data with predictions:")
print(head(test_data))

# -------------------------------------------
# VISUALIZE THE RESULTS
# -------------------------------------------

# 1. Confusion Matrix Heatmap
confusion_matrix_df <- as.data.frame(as.table(bagging_confusion_matrix))
colnames(confusion_matrix_df) <- c("Actual", "Predicted", "Frequency")

ggplot(confusion_matrix_df, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +  # Add frequency labels
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Bagging Classifier Confusion Matrix",
    x = "Actual Category",
    y = "Predicted Category",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12)
  )

# 2. Prediction Scatter Plot
ggplot(test_data, aes(x = Fin_sqft, y = Lotsize, color = Predicted_Category)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Bagging Model Predictions",
    x = "Finished Square Footage",
    y = "Lot Size",
    color = "Predicted Category"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12)
  )


## Models for predict numeric sale price
# Install necessary packages
necessary_packages <- c("rpart", "rpart.plot", "dplyr", "caret", "class", "ggplot2", "e1071", "randomForest", "ipred")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}
lapply(necessary_packages, install_if_missing)

# Load libraries
library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(class)
library(ggplot2)
library(e1071)
library(randomForest)
library(ipred)

# Read the dataset
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Data preprocessing
# Remove rows with missing or zero Sale_price
property_sales_data <- property_sales_data %>% 
  filter(!is.na(Sale_price) & Sale_price > 0)

# Select relevant columns for the model
model_data <- property_sales_data %>%
  select(Sale_price, Year_Built, Fin_sqft, Lotsize, Bdrms, Fbath, Hbath)

# Remove rows with missing values in selected columns
model_data <- na.omit(model_data)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(model_data), 0.7 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# -------------------------------------------
# Linear Regression
# -------------------------------------------
linear_model <- lm(Sale_price ~ ., data = train_data)
linear_predictions <- predict(linear_model, test_data)
linear_rmse <- sqrt(mean((linear_predictions - test_data$Sale_price)^2))

# -------------------------------------------
# Decision Tree Regression
# -------------------------------------------
tree_model <- rpart(Sale_price ~ ., data = train_data, method = "anova")
tree_predictions <- predict(tree_model, test_data)
tree_rmse <- sqrt(mean((tree_predictions - test_data$Sale_price)^2))

# -------------------------------------------
# Random Forest Regression
# -------------------------------------------
rf_model <- randomForest(Sale_price ~ ., data = train_data, ntree = 100)
rf_predictions <- predict(rf_model, test_data)
rf_rmse <- sqrt(mean((rf_predictions - test_data$Sale_price)^2))

# -------------------------------------------
# KNN Regression
# -------------------------------------------
knn_model <- train(
  Sale_price ~ ., 
  data = train_data, 
  method = "knn", 
  tuneGrid = expand.grid(k = 1:10),
  trControl = trainControl(method = "cv")
)
knn_predictions <- predict(knn_model, test_data)
knn_rmse <- sqrt(mean((knn_predictions - test_data$Sale_price)^2))

# -------------------------------------------
# SVM Regression
# -------------------------------------------
svm_model <- svm(Sale_price ~ ., data = train_data, kernel = "radial")
svm_predictions <- predict(svm_model, test_data)
svm_rmse <- sqrt(mean((svm_predictions - test_data$Sale_price)^2))

# -------------------------------------------
# Bagging Regression
# -------------------------------------------
bagging_model <- bagging(
  Sale_price ~ ., 
  data = train_data, 
  coob = TRUE  # Use out-of-bag error for validation
)
bagging_predictions <- predict(bagging_model, test_data)
bagging_rmse <- sqrt(mean((bagging_predictions - test_data$Sale_price)^2))

# -------------------------------------------
# Model Comparison
# -------------------------------------------
model_results <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest", "KNN", "SVM", "Bagging"),
  RMSE = c(linear_rmse, tree_rmse, rf_rmse, knn_rmse, svm_rmse, bagging_rmse)
)

# Print model comparison
print(model_results)

# -------------------------------------------
# Visualize the Results
# -------------------------------------------

# 1. RMSE Comparison Bar Plot
ggplot(model_results, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Model Comparison: RMSE for Sale_Price Prediction",
    x = "Model",
    y = "RMSE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16, face = "bold")
  )

# 2. Scatter Plot of Predicted vs Actual (for the best model, e.g., Random Forest)
best_predictions <- rf_predictions  # Replace with predictions of the best model
test_data$Predicted_Sale_Price <- best_predictions

ggplot(test_data, aes(x = Sale_price, y = Predicted_Sale_Price)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red") +  # Ideal prediction line
  labs(
    title = "Best Model: Actual vs Predicted Sale_Price",
    x = "Actual Sale Price",
    y = "Predicted Sale Price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
#K-MEANS CLUSTERING
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(flexclust)

# 1. Read and preprocess the data -------------------------
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Remove rows with missing or zero Sale_price
property_sales_clean <- property_sales_data %>%
    filter(!is.na(Sale_price) & Sale_price > 0)

# Convert PropType to a numeric factor
property_sales_clean$PropType_numeric <- as.numeric(as.factor(property_sales_clean$PropType))

# Select Sale_price and PropType_numeric for clustering
numeric_data <- property_sales_clean %>%
    select(Sale_price, PropType_numeric)

# 2. Perform K-means clustering ---------------------------
set.seed(435)
kmeans_result <- kmeans(numeric_data, centers = 4, nstart = 25)

# Add cluster assignments to the cleaned data
property_sales_clean$Cluster <- as.factor(kmeans_result$cluster)

# 3. Scatter plot for Sale_price vs PropType --------------
ggplot(property_sales_clean, aes(x = PropType_numeric, y = Sale_price, color = Cluster)) +
    geom_point(alpha = 0.7, size = 3) +
    scale_x_continuous(breaks = 1:length(unique(property_sales_clean$PropType)),
                       labels = unique(property_sales_clean$PropType)) +
    labs(title = "Clusters of Sale Price by Property Type",
         x = "Property Type",
         y = "Sale Price") +
    theme_minimal()
#DBSCAN
install.packages('dbscan')
# Load necessary libraries
library(dplyr)
library(dbscan)      # For density-based clustering
library(ggplot2)     # For visualization

# 1. Read and Preprocess the Data ------------------------------------
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Remove rows with missing or zero Sale_price
property_sales_clean <- property_sales_data %>%
    filter(!is.na(Sale_price) & Sale_price > 0)

# Convert PropType to a numeric factor
property_sales_clean$PropType_numeric <- as.numeric(as.factor(property_sales_clean$PropType))

# Select Sale_price and PropType_numeric for DBSCAN
dbscan_data <- property_sales_clean %>%
    select(Sale_price, PropType_numeric) %>%
    scale()  # Scale the data for better performance

# 2. Apply DBSCAN ----------------------------------------------------
# Set parameters for DBSCAN: eps (neighborhood radius) and minPts (minimum points)
set.seed(123)  # Ensure reproducibility
dbscan_result <- dbscan(dbscan_data, eps = 0.5, minPts = 5)

# Add cluster labels back to the original data
property_sales_clean$Cluster <- as.factor(dbscan_result$cluster)

# 3. Visualize the Clusters ------------------------------------------
ggplot(property_sales_clean, aes(x = PropType_numeric, y = Sale_price, color = Cluster)) +
    geom_point(alpha = 0.7, size = 3) +
    scale_x_continuous(breaks = 1:length(unique(property_sales_clean$PropType)),
                       labels = unique(property_sales_clean$PropType)) +
    labs(title = "DBSCAN Clustering: Sale Price by Property Type",
         x = "Property Type",
         y = "Sale Price") +
    theme_minimal()

#HIERARICHAL CLUSTERING
# Load necessary libraries
library(dplyr)
library(ggplot2)

# 1. Read and Preprocess the Data ------------------------------------
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Remove rows with missing values or zero Sale_price
property_sales_clean <- property_sales_data %>%
  filter(!is.na(Sale_price) & Sale_price > 0 & 
           !is.na(Fin_sqft) & Fin_sqft > 0 & 
           !is.na(Lotsize) & Lotsize > 0) %>%
  select(Sale_price, Fin_sqft, Year_Built, Lotsize)  # Select meaningful features

# Scale the data for clustering
scaled_data <- scale(property_sales_clean)

# 2. Perform Hierarchical Clustering ---------------------------------
# Calculate the distance matrix
distance_matrix <- dist(scaled_data, method = "euclidean")

# Perform hierarchical clustering
hc <- hclust(distance_matrix, method = "ward.D2")

# 3. Plot the Dendrogram --------------------------------------------
# Plot the dendrogram
plot(hc, 
     hang = -1, 
     labels = FALSE, 
     main = "Hierarchical Clustering of Property Sales Data",
     xlab = "Properties", 
     ylab = "Height")

# 4. Assign Clusters to Data ----------------------------------------
# Cut the tree into 4 clusters
property_sales_clean$Cluster <- cutree(hc, k = 4)

# Display cluster summary
print(table(property_sales_clean$Cluster))
