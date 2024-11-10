# LINEAR REGRESSION

# Install required libraries (if not already installed)
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("corrplot")

library(dplyr)
library(ggplot2)
library(corrplot)

# Load the dataset
heart_data <- read.csv("E:/Predictive Analysis Project/12107392_CA3/Ca3predict/heart-disease.csv")

# Check structure of the dataset
str(heart_data)
summary(heart_data)

# Convert categorical columns to factors
heart_data$sex <- as.factor(heart_data$sex)
heart_data$cp <- as.factor(heart_data$cp)  # Example: Chest pain type

# Select a continuous variable for regression (e.g., 'chol' for cholesterol or 'thalach' for max heart rate)
# Replace 'chol' with your chosen target column if different
target_column <- "chol"  # Replace with the name of the continuous target column

# Define the formula for linear regression
linear_formula <- as.formula(paste(target_column, "~ age + sex + cp + trestbps"))

# Train the linear regression model
linear_model <- lm(linear_formula, data = heart_data)

# Display summary of the linear model
summary(linear_model)

# Visualize correlations between features
corr_matrix <- cor(heart_data %>% select_if(is.numeric))
corrplot(corr_matrix, method = "circle", type = "upper", tl.col = "blue", tl.srt = 45)

# Plot the relationship between target variable and one predictor, e.g., 'age' vs 'chol'
ggplot(heart_data, aes(x = age, y = chol)) +
  geom_point(aes(color = cp), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Linear Regression: Cholesterol vs Age",
       x = "Age",
       y = "Cholesterol") +
  theme_minimal()

