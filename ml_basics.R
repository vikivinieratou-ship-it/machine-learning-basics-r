# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create a small dataset with car features and prices
car_data <- data.frame(
  Horsepower = c(130, 150, 180, 200, 110, 140, 170),
  Weight = c(2100, 1950, 3050, 4000, 2700, 2900, 3200),
  ltper100km = c(8.5, 7.9, 11, 13, 9, 9.8, 12),
  Price = c(20000, 23000, 27000, 30000, 19000, 21000, 26000)
)

# Print the dataset
print(car_data)

# Train a multiple regression model
model <- lm(Price ~ Horsepower + Weight + ltper100km, data = car_data)

# Print the model summary
summary(model)

# Residuals
residuals <- model$residuals
print(residuals)

# Predict car price for a new car with specific features
new_car <- data.frame(Horsepower = 170, Weight = 3150, ltper100km = 12)
predicted_price <- predict(model, newdata = new_car)
print(predicted_price)

library(scatterplot3d)

# Create the 3D scatter plot
scatterplot3d(car_data$Horsepower, car_data$Weight, car_data$ltper100km, 
              color = "blue", pch = 16, main = "3D Scatter Plot of Car Data")
#b

# Dataset with 2 features (x1, x2) and binary labels (y)
X <- cbind(c(0, 1, 0, 1, 1, 2), c(0, 0, 1, 1, 2, 1))  # 6 samples, 2 features
y <- c(0, 0, 1, 1, 1, 1)    # Binary class labels

# Define the ReLU activation function
relu <- function(x) {
  return(pmax(0, x))  # Return max(0, x)
}

# Function to compute the predicted output using ReLU activation
neuron_calc <- function(x, w, b) {
  return(relu(sum(x * w) + b))  # Weighted sum of inputs + bias, then apply ReLU
}
# Example weights and bias
w <- c(0.1, -0.2)  # Example weights for the two features
b <- 0.5  # Example bias

# Define the ReLU activation function
relu <- function(x) {
  return(pmax(0, x))  # Return max(0, x)
}

# Function to compute the predicted output using ReLU activation
neuron_calc <- function(x, w, b) {
  return(relu(sum(x * w) + b))  # Weighted sum of inputs + bias, then apply ReLU
}

# Define the input values for which we need to compute the output
inputs <- list(
  c(0.5, 0.5),
  c(0.75, 0.75),
  c(0.5, 0.9),
  c(0.2, 0.5),
  c(0.7, 1.3),
  c(2, 0),
  c(0.5, 0.75),
  c(1.25, 0)
)

# Calculate the outputs for each input
outputs <- sapply(inputs, function(x) neuron_calc(x, w, b))

# Print the outputs
print(outputs)

#c
# Update weights and bias function
update_w <- function(X, delta, w, b, learning_rate) {
  # Gradient for weights (one per feature)
  dw <- rep(0, ncol(X))  # Gradient for each feature's weight
  db <- 0  # Gradient for bias
  
  # Calculate the gradients
  for (i in 1:nrow(X)) {
    dw <- dw + delta[i] * X[i, ]
    db <- db + delta[i]
  }
  
  # Update weights and bias
  w <- w + learning_rate * dw
  b <- b + learning_rate * db
  
  return(list(w = w, b = b))
}

#d

# Initialize random weights and bias
set.seed(42)  # For reproducibility
w <- matrix(runif(2, -0.1, 0.1), ncol = 1)  # Random weights for two features 
b <- runif(1, -0.1, 0.1)  # Random bias 
learning_rate <- 0.01 
epochs <- 100  # Number of training epochs

# Define relu and neuron_calc again for context
relu <- function(x) {
  return(pmax(0, x))
}

neuron_calc <- function(x, w, b) {
  return(relu(sum(x * w) + b))
}

update_w <- function(X, delta, w, b, learning_rate) {
  dw <- rep(0, ncol(X))
  db <- 0
  for (i in 1:nrow(X)) {
    dw <- dw + delta[i] * X[i, ]
    db <- db + delta[i]
  }
  w <- w + learning_rate * dw
  b <- b + learning_rate * db
  return(list(w = w, b = b))
}

# Training loop
for (epoch in 1:epochs) {
  y_pred <- numeric(nrow(X))
  for (i in 1:nrow(X)) {
    y_pred[i] <- neuron_calc(X[i, ], w, b)
  }
  
  delta <- y - y_pred  # <- insert your code here
  
  # Update weights and bias
  result <- update_w(X, delta, w, b, learning_rate)  # <- insert your code here
  w <- result$w
  b <- result$b
}

# Final weights and bias
cat("Final weights:", round(w, 3), "\n")
cat("Final bias:", round(b, 3), "\n")

# Prediction for test point (1, 2)
test_X <- c(1, 2)
prediction <- neuron_calc(test_X, w, b)
cat("Prediction for test point (1, 2):", round(prediction, 1), "\n")
cat("Step function output:", step_function(prediction), "\n")

# Print logged weights and biases
print(epoch_log)

