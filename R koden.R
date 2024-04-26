if (!require("e1071")) install.packages("e1071", dependencies=TRUE)
library(e1071)
# Load the data
file_path <- "C:/Users/Min Dator/Downloads/data_bil 3.xlsx"
car_data <- read_excel(file_path)

# Convert necessary columns to factor type (assuming 'Brand', 'Fuel', 'Gearbox' are categorical)
car_data$Brand <- as.factor(car_data$Brand)
car_data$Fuel <- as.factor(car_data$Fuel)
car_data$Gearbox <- as.factor(car_data$Gearbox)

# Splitting the data (randomly select 80% of the data for training)
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(car_data), 0.8 * nrow(car_data))
train_data <- car_data[train_indices, ]
test_data <- car_data[-train_indices, ]


# Train the SVM model
svm_model <- svm(Price ~ ., data = train_data, kernel = "radial", cost = 10, gamma = 0.1)

# Summarize the model
summary(svm_model)


# Predicting prices
svm_predictions <- predict(svm_model, test_data)

# Calculate Mean Squared Error
svm_mse <- mean((svm_predictions - test_data$Price)^2)
print(paste("SVM Mean Squared Error:", svm_mse))

# Calculate Mean Absolute Error
svm_mae <- mean(abs(svm_predictions - test_data$Price))
print(paste("SVM Mean Absolute Error:", svm_mae))



library(ggplot2)
comparison_data <- data.frame(Actual = test_data$Price, Predicted = svm_predictions)
ggplot(comparison_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, col = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Prices: SVM Model", x = "Actual Price", y = "Predicted Price") +
  theme_minimal()














# Install required packages if they are not already installed
if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("e1071")) install.packages("e1071", dependencies=TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)

library(readxl)
library(e1071)
library(dplyr)

# Load the data
file_path <- "C:/Users/Min Dator/Downloads/data_bil 3.xlsx"
car_data <- read_excel(file_path)

# Ensure factor variables are correctly typed
car_data$Brand <- as.factor(car_data$Brand)
car_data$Fuel <- as.factor(car_data$Fuel)
car_data$Gearbox <- as.factor(car_data$Gearbox)

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(car_data), 0.8 * nrow(car_data), replace = FALSE)
train_data <- car_data[train_indices, ]
test_data <- car_data[-train_indices, ]

# Train the SVM model
svm_model <- svm(Price ~ ., data = train_data, kernel = "radial", cost = 10, gamma = 0.1)

# Predicting prices using the trained SVM model
svm_predictions <- predict(svm_model, test_data)

# Create a data frame to compare actual and predicted prices
comparison_df <- data.frame(Actual_Price = test_data$Price, Predicted_Price = svm_predictions)
comparison_df <- mutate(comparison_df, Difference = Actual_Price - Predicted_Price)

# Output the comparison data frame
print(comparison_df)

# Calculate and print Mean Squared Error and Mean Absolute Error
mse <- mean((svm_predictions - test_data$Price)^2)
mae <- mean(abs(svm_predictions - test_data$Price))

cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")











