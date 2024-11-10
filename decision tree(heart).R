# Load necessary libraries
library(rpart)
library(rpart.plot)

# Load the dataset
df <- read.csv("E:/Predictive Analysis Project/12107392_CA3/Ca3predict/heart-disease.csv")

# Display structure to confirm column names and types
str(df)

# Update with the actual target column name (e.g., 'target' if it's different from 'diagnosis')
target_column <- "target"  # Replace with actual name based on str(df)

# Convert relevant columns to factors if they are categorical (based on actual structure)
df$sex <- as.factor(df$sex)
# Add more conversions if other columns need to be factors

# Split the data into training and testing sets (80% train, 20% test)
set.seed(1234)
pd <- sample(2, nrow(df), replace = TRUE, prob = c(0.8, 0.2))
tr <- df[pd == 1, ]
ts <- df[pd == 2, ]

# Define formula dynamically based on target column
formula <- as.formula(paste(target_column, "~ ."))

# Train the decision tree model on training data
dstree1 <- rpart(formula, data = tr, method = "class")

# Plot the decision tree
rpart.plot(dstree1)

# Predict on test data
predictions <- predict(dstree1, ts, type = "class")

# Confusion matrix and accuracy calculation
tab <- table(predictions, ts[[target_column]])
accuracy <- sum(diag(tab)) / sum(tab)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

