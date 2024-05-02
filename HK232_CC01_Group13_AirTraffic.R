# Update all system libraries of R
update.packages(ask = FALSE)

# Check information about the data
head(HK232_CC01_Group13_AirTraffic)
str(HK232_CC01_Group13_AirTraffic)
nrow(HK232_CC01_Group13_AirTraffic)

# Set seed for reproducibility
set.seed(123)

# Generate random indices for data partitioning
indices <- sample(1:nrow(HK232_CC01_Group13_AirTraffic))
train_indices <- indices[1:round(0.7 * length(indices))]
test_indices <- indices[(round(0.7 * length(indices)) + 1):length(indices)]

# Split the data into training and testing sets
train_data <- HK232_CC01_Group13_AirTraffic[train_indices, ]
test_data <- HK232_CC01_Group13_AirTraffic[test_indices, ]

# Build an initial logistic regression model with all independent variables
model <- glm(`Price Category Code` ~ 
               `Operating Airline` + 
               `Published Airline` + 
               `GEO Summary` +
               `GEO Region` +
               `Activity Type Code` +
               `Terminal` +
               `Boarding Area` +
               `Passenger Count` +
               `Adjusted Passenger Count` +
               `Year` +
               `Month`,
             data = train_data, family = "binomial")

# Perform stepwise model selection based on AIC
library(stats)
library(MASS)
step_model <- stepAIC(model, direction = "both")

# Print the summary of the final selected model
summary(step_model)

# Predict probabilities
predicted <- predict(model, type = "response")

# Plot the logistic regression curve
library(ggplot2)
ggplot(data = train_data, aes(x = `Operating Airline`, y = `Price Category Code`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_line(aes(y = predicted), color = "red") +
  labs(x = "Operating Airline", y = "Price Category Code")
ggplot(data = train_data, aes(x = `Published Airline`, y = `Price Category Code`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_line(aes(y = predicted), color = "yellow") +
  labs(x = "Published Airline", y = "Price Category Code")
ggplot(data = train_data, aes(x = `GEO Summary`, y = `Price Category Code`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_line(aes(y = predicted), color = "orange") +
  labs(x = "GEO Summary", y = "Price Category Code")
ggplot(data = train_data, aes(x = `GEO Region`, y = `Price Category Code`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_line(aes(y = predicted), color = "green") +
  labs(x = "GEO Region", y = "Price Category Code")
ggplot(data = train_data, aes(x = `Boarding Area`, y = `Price Category Code`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_line(aes(y = predicted), color = "blue") +
  labs(x = "Boarding Area", y = "Price Category Code")
ggplot(data = train_data, aes(x = `Passenger Count`, y = `Price Category Code`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_line(aes(y = predicted), color = "purple") +
  labs(x = "Passenger Count", y = "Price Category Code")
ggplot(data = train_data, aes(x = `Adjusted Passenger Count`, y = `Price Category Code`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_line(aes(y = predicted), color = "pink") +
  labs(x = "Adjusted Passenger Count", y = "Price Category Code")
ggplot(data = train_data, aes(x = `Year`, y = `Price Category Code`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_line(aes(y = predicted), color = "black") +
  labs(x = "Year", y = "Price Category Code")

# Make predictions on test data
test_predicted <- predict(step_model, newdata = test_data, type = "response")

# Convert predicted probabilities to predicted classes (0 or 1)
test_predicted_classes <- ifelse(test_predicted > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(test_predicted_classes == test_data$`Price Category Code`)
cat("Accuracy:", accuracy, "\n")

