#KNN
# Load necessary libraries
library(caTools)   # For splitting the data
library(class)     # For KNN
library(ggplot2)   # For plotting
library(reshape2)  # For reshaping data for plotting

# Load the dataset
df <- read.csv("E:/Predictive Analysis Project/12107392_CA3/Ca3predict/heart-disease.csv")  # Ensure this path is correct

# Check the structure of the dataset
str(df)

# Print column names to verify what is available
print(colnames(df))

# Check for missing columns before conversion
if ("sex" %in% colnames(df)) {
  df$sex <- as.numeric(as.factor(df$sex))
}

if ("cp" %in% colnames(df)) {
  df$cp <- as.numeric(as.factor(df$cp))  # Chest pain type
}

if ("target" %in% colnames(df)) {
  df$target <- as.factor(df$target)        # Target variable (heart disease presence)
}

if ("sample_origin" %in% colnames(df)) {
  df$sample_origin <- as.numeric(as.factor(df$sample_origin))
}

if ("patient_cohort" %in% colnames(df)) {
  df$patient_cohort <- as.numeric(as.factor(df$patient_cohort))
}

if ("stage" %in% colnames(df)) {
  df$stage <- as.numeric(as.factor(df$stage))
}

# Check structure again after preprocessing
str(df)

# Select relevant features (adjust based on actual column names)
# Use only the columns that exist in the dataframe
selected_columns <- c("sex", "cp", "sample_origin", "patient_cohort", "stage", "target")
selected_columns <- selected_columns[selected_columns %in% colnames(df)]  # Filter to existing columns

# Create a new dataframe with selected columns
df <- df[, selected_columns]  

# Normalize the dataset (scale numeric features)
df[, -which(names(df) == "target")] <- scale(df[, -which(names(df) == "target")])  # Exclude target for scaling

# Set seed for reproducibility
set.seed(1234)

# Split data into training and testing sets
pd <- sample(2, nrow(df), replace = TRUE, prob = c(0.75, 0.25))
tr <- df[pd == 1, ]
ts <- df[pd == 2, ]

# Ensure lengths match before training KNN
if (nrow(tr) > 0 && nrow(ts) > 0) {
  # Train the KNN classifier
  k_value <- 5  # You can tune this value for better performance
  classifier_knn <- knn(train = tr[, -which(names(tr) == "target")],   # Exclude the target variable
                        test = ts[, -which(names(ts) == "target")],     # Exclude the target variable
                        cl = tr$target,                                  # Class labels
                        k = k_value)
  
  # Confusion Matrix
  cm <- table(ts$target, classifier_knn)
  print(cm)
  
  # Calculate accuracy
  accuracy <- sum(diag(cm)) / sum(cm)
  print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
  
  # Plot confusion matrix heatmap
  # Plot confusion matrix heatmap
  cm_melted <- melt(cm)
  colnames(cm_melted) <- c("True", "Predicted", "Count")  # Rename columns for clarity
  
  ggplot(data = cm_melted, aes(x = Predicted, y = True, fill = Count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "skyblue", high = "red") +
    labs(x = "Predicted Label", y = "True Label", fill = "Count") +
    ggtitle(paste("Confusion Matrix (Accuracy:", round(accuracy * 100, 2), "%)")) +
    theme_minimal()
  
  
} else {
  print("Training or test dataset is empty!")
}

