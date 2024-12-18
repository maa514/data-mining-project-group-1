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
library(caret)
library(rpart)
library(rpart.plot)

# Train the decision tree with cross-validation
control <- trainControl(method = "cv", number = 5)

# Train the decision tree model
dt_model <- train(
  Price_Category ~ ., 
  data = train_data,
  method = "rpart",
  trControl = control,
  tuneLength = 10  # Automatically tune parameters
)

# Print the best tuning parameters
print(dt_model$bestTune)

# Visualize the decision tree
rpart.plot(dt_model$finalModel, main = "Optimized Decision Tree", type = 3)

# Evaluate the model's performance
dt_predictions <- predict(dt_model, test_data)
confusion_matrix_dt <- confusionMatrix(dt_predictions, test_data$Price_Category)
print(confusion_matrix_dt)



## KNN algorithm
# Load necessary libraries
library(caret)
library(class)
library(dplyr)
library(ggplot2)

# Load the dataset
property_sales_data <- read.csv("2002-2018-property-sales-data.csv")

# Data Preprocessing: Remove rows with missing or zero Sale_price
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

# Split data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(model_data), 0.7 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Tuning the value of k using caret and cross-validation
control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train KNN model with cross-validation over different values of k
knn_model <- train(
  Price_Category ~ ., 
  data = train_data,
  method = "knn",
  trControl = control,
  tuneLength = 20  # Test 20 different values for k
)

# Print the best k and its results
print(knn_model$bestTune)

# Visualize the performance of KNN for each value of k
ggplot(knn_model$results, aes(x = factor(k), y = Accuracy)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  labs(
    title = "KNN Accuracy for Different k Values",
    x = "Number of Neighbors (k)",
    y = "Accuracy"
  ) +
  theme_minimal()

# Evaluate the optimized KNN model on the test data
knn_predictions <- predict(knn_model, test_data)

# Add predictions to the test data for easier comparison
test_data$Predicted_Price_Category <- knn_predictions

# Show the actual vs predicted values
print("Actual vs Predicted Values:")
head(test_data[, c("Price_Category", "Predicted_Price_Category")])

# Confusion Matrix for the test data
confusion_matrix <- table(test_data$Price_Category, knn_predictions)

# Print confusion matrix and accuracy
print("Confusion Matrix:")
print(confusion_matrix)
knn_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Optimized KNN Accuracy:", round(knn_accuracy, 4)))

# Plot confusion matrix heatmap for better visualization
confusion_matrix_df <- as.data.frame(as.table(confusion_matrix))
colnames(confusion_matrix_df) <- c("Actual", "Predicted", "Frequency")

ggplot(confusion_matrix_df, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frequency), color = "black", size = 5) +  # Add labels to cells
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "KNN Confusion Matrix Heatmap",
    x = "Actual Category",
    y = "Predicted Category",
    fill = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12)
  )

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
#--------------------------
#K-MEANS CLUSTERING
#--------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the dataset (adjust path as needed)
data <- read.csv("2002-2018-property-sales-data.csv")

# Filter necessary columns
data_filtered <- data %>%
  filter(PropType %in% c("Commercial", "Condominium", "Lg Apartment", 
                         "Vacant Land", "Residential")) %>%
  select(PropType, Sale_price, Fin_sqft) %>%
  filter(Sale_price > 0 & Fin_sqft > 0)

# Scale the Sale_price and Fin_sqft for K-means
data_scaled <- scale(data_filtered[, c("Sale_price", "Fin_sqft")])

# Perform K-means clustering
set.seed(123) # For reproducibility
k <- 5 # Number of clusters representing the 5 property types
kmeans_result <- kmeans(data_scaled, centers = k)

# Add cluster results back to the original data
data_filtered$Cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters
ggplot(data_filtered, aes(x = Fin_sqft, y = Sale_price, color = Cluster)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "K-means Clustering of Sale Price vs. Finished Square Feet",
       x = "Finished Square Feet",
       y = "Sale Price",
       color = "Cluster") +
 theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Cluster mapping to property types
cluster_summary <- data_filtered %>%
  group_by(Cluster) %>%
  summarize(Average_Sale_Price = mean(Sale_price),
            Average_Fin_sqft = mean(Fin_sqft))

print(cluster_summary)

#-----------------------
#DBSCAN CLUSTERING
#-----------------------
                            
# Load required libraries
library(dplyr)
library(dbscan)
library(ggplot2)

# Read the dataset
data <- read.csv("2002-2018-property-sales-data.csv")

# Step 1: Data Preparation
data_clean <- data %>%
  filter(!is.na(Sale_price) & Sale_price > 0,       # Valid Sale Price
         !is.na(Fin_sqft) & Fin_sqft > 0) %>%       # Valid Finished Square Feet
  filter(PropType %in% c("Commercial", "Condominium", "Lg Apartment", 
                         "Vacant Land", "Residential")) %>%  # Relevant property types
  select(PropType, Sale_price, Fin_sqft)

# Step 2: Perform DBSCAN
set.seed(123) # For reproducibility

# Select the features for clustering
data_features <- data_clean %>%
  select(Fin_sqft, Sale_price)

# Scale the data for DBSCAN
data_scaled <- scale(data_features)

# Run DBSCAN
dbscan_result <- dbscan(data_scaled, eps = 0.5, minPts = 5)

# Step 3: Add Clusters to Data
data_clean$Cluster <- as.factor(dbscan_result$cluster)

# Step 4: Cluster Analysis
cluster_summary <- data_clean %>%
  group_by(Cluster, PropType) %>%
  summarize(
    Avg_Sale_Price = mean(Sale_price),
    Avg_Fin_Sqft = mean(Fin_sqft),
    Count = n(),
    .groups = "drop"
  )
print(cluster_summary)

# Step 5: Visualization
ggplot(data_clean, aes(x = Fin_sqft, y = Sale_price, color = Cluster)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "DBSCAN Clustering of Sale Price vs Finished Square Feet",
       x = "Finished Square Feet",
       y = "Sale Price",
       color = "Cluster") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

#----------------------
#HIERARICHAL CLUSTERING
#----------------------
                            
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Load and clean the data
data <- read.csv("2002-2018-property-sales-data.csv")

# Filter valid data and select relevant features
data_clean <- data %>%
  filter(Sale_price > 0 & Fin_sqft > 0 & !is.na(Year_Built)) %>%
  select(PropType, Sale_price, Fin_sqft, Year_Built, Lotsize, Nr_of_rms)

# Step 2: Scale the data
data_scaled <- scale(data_clean[, c("Sale_price", "Fin_sqft", "Year_Built", "Lotsize", "Nr_of_rms")])

# Step 3: Perform hierarchical clustering
set.seed(123)
hc <- hclust(dist(data_scaled), method = "average")

# Step 4: Plot a subset of the data
sample_idx <- sample(1:nrow(data_clean), 50) # Take a random sample of 50 points
plot(hclust(dist(data_scaled[sample_idx, ]), method = "average"), 
     labels = data_clean$PropType[sample_idx],
     main = "Hierarchical Clustering (Sampled Data)",
     xlab = "Property Type", ylab = "Height", hang = -1, cex = 0.7)
