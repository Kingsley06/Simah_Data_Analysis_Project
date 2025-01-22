
# Simah Exclusive Collections Analysis Project
# Author: Kingsley Ayozie
# Date: January 2025

# Load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(caret)
library(corrplot)

# Data Import and Cleaning ----------------------------------------------------
# Import the dataset
data <- read.csv("simah_exclusive_collections.csv")

# Inspect the dataset structure
str(data)

# Handle missing values
data <- na.omit(data)

# Convert date column to Date type
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Feature Engineering ---------------------------------------------------------
# Add month and year columns for temporal analysis
data$Month <- month(data$Date, label = TRUE, abbr = TRUE)
data$Year <- year(data$Date)

# Create a sales growth column
data <- data %>% arrange(Date) %>% mutate(Sales_Growth = (Revenue - lag(Revenue)) / lag(Revenue) * 100)

# Data Analysis and Insights --------------------------------------------------
# Identify top-performing products
top_products <- data %>%
  group_by(Product) %>%
  summarize(Total_Revenue = sum(Revenue)) %>%
  arrange(desc(Total_Revenue))

# Visualize top-performing products
ggplot(top_products, aes(x = reorder(Product, -Total_Revenue), y = Total_Revenue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top-Performing Products", x = "Product", y = "Total Revenue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Seasonal demand forecasting
sales_by_month <- data %>%
  group_by(Month) %>%
  summarize(Average_Revenue = mean(Revenue))

ggplot(sales_by_month, aes(x = Month, y = Average_Revenue, group = 1)) +
  geom_line(color = "darkorange", size = 1) +
  geom_point(size = 3, color = "darkorange") +
  labs(title = "Seasonal Demand Trend", x = "Month", y = "Average Revenue")

# Correlation analysis
cor_matrix <- cor(data %>% select_if(is.numeric))
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Predictive Analytics --------------------------------------------------------
# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$Revenue, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train a linear regression model
lm_model <- lm(Revenue ~ ., data = train_data)

# Model summary
summary(lm_model)

# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Evaluate model performance
rmse <- sqrt(mean((test_data$Revenue - predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Data Visualization ----------------------------------------------------------
# Revenue trends over time
ggplot(data, aes(x = Date, y = Revenue)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Revenue Trend Over Time", x = "Date", y = "Revenue")

# Save outputs and visualizations
ggsave("top_products.png")
ggsave("seasonal_trend.png")

# Export cleaned data
write.csv(data, "cleaned_simah_data.csv", row.names = FALSE)
