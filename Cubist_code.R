# Load necessary library
library(Cubist)

# Generate a list of functions
functions <- list(
  function(x) sin(x) * exp(-x / (6 * pi)),           # Function 1
  function(x) cosh(x / 10) - sinh(x / 15),           # Function 2 (Hyperbolic functions)
  function(x) tan(x / 2) / (1 + abs(x)),             # Function 3
  function(x) x^3 - 4 * x^2 + x + 6,                 # Function 4 (Polynomial)
  function(x) sqrt(abs(cos(x))) * log(x + 1),        # Function 5
  function(x) sin(x)^2 * cos(x)^2,                   # Function 6
  function(x) exp(-x),                               # Function 7 (exp(-x) type behavior)
  function(x) x^2 - 5 * x + 4,                       # Function 8 (Polynomial)
  function(x) tanh(x / 5) * sinh(x / 10),            # Function 9 (Hyperbolic functions)
  function(x) sin(x) * cos(x) - log(x + 1) / (1 + x^2) # Function 10
)

# Initialize a data frame to store results
results <- data.frame(
  Function = integer(),
  Rules = integer(),
  MSE = numeric(),
  PointsBelowCurve = numeric(),
  PercentageBelowCurve = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each function
for (f_index in seq_along(functions)) {
  # Generate data for the current function
  set.seed(123 + f_index)  # Ensure reproducibility
  x <- seq(0, 8 * pi, length.out = 100)  # Generate x values
  y <- functions[[f_index]](x)           # Compute y values
  data <- data.frame(x = x, y = y)       # Combine into a data frame
  
  # Loop through the number of rules from 1 to 5
  for (rule_count in 1:5) {
    cat("Processing Function", f_index, "with rules =", rule_count, "\n")
    
    # Initialize weights
    weights <- rep(1, nrow(data))  # Equal weights initially
    
    # Iterative process to approach the upper bound
    for (i in 1:150) {  # Fixed iterations (120)
      # Fit a Cubist model with the specified number of rules and weights
      model <- cubist(
        x = as.data.frame(data$x), 
        y = data$y, 
        weights = weights, 
        control = cubistControl(rules = rule_count)
      )
      
      # Predict values
      y_pred <- predict(model, newdata = as.data.frame(data$x))
      
      # Adjust weights
      weights <- ifelse(data$y >= y_pred, weights * 1.02, weights * 0.98)  
      weights <- weights / max(weights)  # Normalize weights
    }
    
    # Final predictions
    y_pred <- predict(model, newdata = as.data.frame(data$x))
    
    # Calculate residuals
    residuals <- data$y - y_pred
    
    # Identify points where predictions fall below the curve
    below_curve_indices <- which(residuals > 0)  # Indices where residuals are positive
    
    # Calculate metrics
    num_below_curve <- length(below_curve_indices)  # Number of points below the curve
    total_points <- nrow(data)                      # Total number of points
    percentage_below_curve <- (num_below_curve / total_points) * 100  # Percentage
    final_residuals <- pmax(residuals, 0)           # Penalize only where predictions are below actual
    mse <- mean(final_residuals^2)                 # Final Mean Squared Error
    
    # Append results to the data frame
    results <- rbind(results, data.frame(
      Function = f_index,
      Rules = rule_count,
      MSE = mse,
      PointsBelowCurve = num_below_curve,
      PercentageBelowCurve = percentage_below_curve
    ))
    
    # Save plot as an image
    png_filename <- paste0("Function_", f_index, "Rules", rule_count, ".png")
    png(png_filename, width = 800, height = 600)  # Open PNG device
    
    # Create the plot
    plot(data$x, data$y, type = "l", col = "blue", lwd = 2, 
         main = paste("Function", f_index, "- Rules:", rule_count),
         xlab = "x", ylab = "y")
    lines(data$x, y_pred, col = "red", lwd = 2)  # Prediction line
    points(data$x[below_curve_indices], data$y[below_curve_indices], 
           col = "green", pch = 19, cex = 1.5)  # Highlight points below the curve
    legend("topright", legend = c("Original", "Prediction", "Below Curve"), 
           col = c("blue", "red", "green"), lty = c(1, 1, NA), pch = c(NA, NA, 19), lwd = 2)
    
    dev.off()  # Close PNG device
  }
}

# Print the summary table
cat("Summary of Results:\n")
print(results)

# Save results to a CSV file
write.csv(results, "Cubist_Summary_Results.csv", row.names = FALSE)

cat("Results have been saved to 'Cubist_Summary_Results.csv'.\n")

list.files(pattern = "\\.png$")
getwd()

