# Installing packages
install.packages("readr")
library(readr)
install.packages("corrplot")
library(corrplot)

# Reading the data set into R
property_sales_data <- read_csv("2002-2018-property-sales-data.csv")

# Check for missing values
colSums(is.na(property_sales_data))

## basic data visualizations
# Create histogram with adjusted x-axis limits
ggplot(property_sales_data, aes(x = Sale_price)) +
  geom_histogram(binwidth = 50000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Housing Prices", x = "Sale Price", y = "Count") +
  xlim(0, 5000000) +  # Adjust this value based on your data
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
library(caret)
preProc <- preProcess(numeric_data, method = c("center", "scale"))
data_normalized <- predict(preProc, numeric_data)

summary(numeric_data)          # Original data
summary(data_normalized)       # Normalized data

# data_normalized can be used for PCA




