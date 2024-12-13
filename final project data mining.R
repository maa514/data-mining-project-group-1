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
# Load necessary libraries
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("caret")
install.packages("class")
install.packages("ggplot2")

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

# Convert data to matrices for KNN
train_labels <- train_data$Price_Category
test_labels <- test_data$Price_Category
train_features <- train_data[, -1]
test_features <- test_data[, -1]

# Find the optimal k for KNN
accuracy_results <- data.frame(K = integer(), Accuracy = numeric())

for (k in 1:20) {
  knn_predictions <- knn(
    train = train_features,
    test = test_features,
    cl = train_labels,
    k = k
  )
  confusion_matrix <- table(test_labels, knn_predictions)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  accuracy_results <- rbind(accuracy_results, data.frame(K = k, Accuracy = accuracy))
}

# Find the best k
best_k <- accuracy_results[which.max(accuracy_results$Accuracy), ]
print(paste("Best K:", best_k$K))
print(paste("Best Accuracy:", round(best_k$Accuracy, 4)))

# Plot accuracy vs. k
ggplot(accuracy_results, aes(x = K, y = Accuracy)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "KNN Accuracy vs. K",
    x = "Number of Neighbors (K)",
    y = "Accuracy"
  ) +
  theme_minimal()

# Evaluate KNN with the best k
knn_predictions_best <- knn(
  train = train_features,
  test = test_features,
  cl = train_labels,
  k = best_k$K
)
best_confusion_matrix <- table(test_labels, knn_predictions_best)

# Print confusion matrix and accuracy for the best k
print("Best KNN Confusion Matrix:")
print(best_confusion_matrix)
best_accuracy <- sum(diag(best_confusion_matrix)) / sum(best_confusion_matrix)
print(paste("Best KNN Accuracy:", round(best_accuracy, 4)))

## SVM Model
# Load necessary libraries
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("caret")
install.packages("class")
install.packages("ggplot2")
install.packages("e1071")

library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(class)
library(ggplot2)
library(e1071)

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

# Build and tune SVM model
svm_tune <- tune.svm(
  Price_Category ~ ., 
  data = train_data, 
  kernel = "radial",
  cost = 2^(-1:2), 
  gamma = 2^(-2:2),
  scale = FALSE
)

# Get the best parameters
best_svm_model <- svm_tune$best.model
print("Best SVM Parameters:")
print(best_svm_model)

# Evaluate the best SVM model
svm_predictions <- predict(best_svm_model, test_data)
svm_confusion_matrix <- table(test_data$Price_Category, svm_predictions)

# Print confusion matrix and accuracy
print("SVM Confusion Matrix:")
print(svm_confusion_matrix)
svm_accuracy <- sum(diag(svm_confusion_matrix)) / sum(svm_confusion_matrix)
print(paste("SVM Accuracy:", round(svm_accuracy, 4)))

# Plot SVM performance
accuracy_results <- data.frame(
  Cost = svm_tune$performances$cost,
  Gamma = svm_tune$performances$gamma,
  Accuracy = 1 - svm_tune$performances$error
)

ggplot(accuracy_results, aes(x = Cost, y = Gamma, fill = Accuracy)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "SVM Accuracy by Cost and Gamma",
    x = "Cost",
    y = "Gamma"
  ) +
  theme_minimal()

## Rule-based classifier
# Load necessary libraries
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("caret")
install.packages("class")
install.packages("ggplot2")
install.packages("e1071")

library(rpart)
library(rpart.plot)
library(dplyr)
library(caret)
library(class)
library(ggplot2)
library(e1071)

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

# Split data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(model_data), 0.7 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Build a rule-based classifier
rule_based_classifier <- function(row) {
  if (row$Fin_sqft > 3000 & row$Lotsize > 5000) {
    return("High")
  } else if (row$Fin_sqft > 1500 & row$Bdrms >= 3) {
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

## Bagging
# Load necessary libraries
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("caret")
install.packages("class")
install.packages("ggplot2")
install.packages("e1071")
install.packages("ipred")

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
head(test_data)
