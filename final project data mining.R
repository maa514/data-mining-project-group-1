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
