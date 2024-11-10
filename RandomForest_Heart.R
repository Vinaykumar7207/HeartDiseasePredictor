# RANDOM FOREST

# Install required library (if not already installed)
# install.packages("randomForest")
library(randomForest)

# Load the dataset
heart_data <- read.csv("E:/Predictive Analysis Project/12107392_CA3/Ca3predict/heart-disease.csv")

# Check the structure and summary of the dataset
str(heart_data)
summary(heart_data)

# Convert categorical columns to factors
heart_data$sex <- as.factor(heart_data$sex)
heart_data$cp <- as.factor(heart_data$cp)  # Example: Chest pain type
heart_data$target <- as.factor(heart_data$target)  # Target variable (e.g., presence of heart disease)

# Set a seed for reproducibility
set.seed(1234)

# Fit the random forest model
# Replace 'target' with the actual name of the target column in your dataset
random_forest_model <- randomForest(target ~ age + sex + cp + trestbps + chol + thalach, 
                                    data = heart_data, 
                                    ntree = 100,  # Number of trees
                                    mtry = 3,    # Number of variables randomly sampled at each split
                                    importance = TRUE)

# Print model summary
print(random_forest_model)

# Plot variable importance
varImpPlot(random_forest_model, main = "Variable Importance in Random Forest for Heart Disease Prediction")

