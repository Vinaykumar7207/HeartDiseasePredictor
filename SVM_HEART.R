#SVM
# Load necessary libraries
library(caTools)
library(e1071)
library(ggplot2)  # For the pie chart

# Load the heart disease dataset
df <- read.csv("E:/Predictive Analysis Project/12107392_CA3/Ca3predict/heart-disease.csv")

# Check the structure of the dataset to confirm column names and data types
str(df)

# Convert categorical columns to factors or integers as needed
df$sex <- as.integer(as.factor(df$sex))
df$cp <- as.integer(as.factor(df$cp))         # Example of a categorical column
df$target <- as.factor(df$target)             # Target variable

# Drop any unnecessary columns (e.g., ID column if present)
df <- df[, -c(1)]  # Adjust as needed based on your dataset

# Set seed for reproducibility
set.seed(777)

# Split data into training and testing sets
split <- sample.split(df$target, SplitRatio = 0.8)
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

# Check structures to ensure they match
str(train_data)
str(test_data)

# Train the SVM classifier with a linear kernel
classifier <- svm(formula = target ~ ., 
                  data = train_data, 
                  type = 'C-classification', 
                  kernel = 'linear')

# Predict on test set
predictions <- predict(classifier, newdata = test_data)

# Generate a confusion matrix
cm <- table(predictions, test_data$target)
print(cm)

# Calculate accuracy
accuracy <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Pie chart to visualize the distribution of predictions and actual targets
cm_df <- as.data.frame(cm)
colnames(cm_df) <- c("Predicted", "Actual", "Count")

# Plot the pie chart for the confusion matrix
ggplot(cm_df, aes(x = "", y = Count, fill = interaction(Predicted, Actual))) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  theme_void() + 
  labs(title = "Pie Chart of Confusion Matrix (Predicted vs Actual)")

