# Load necessary libraries
library(ggplot2)
library(MASS)  # For generating Poisson distributed data

# Set seed for reproducibility
set.seed(123)

# Simulate data
income <- rnorm(100, mean = 50000, sd = 10000)  # Simulating income data
happiness <- rpois(100, lambda = 0.00002 * income^2)  # Simulating happiness data (quadratic relationship with income)

# Create data frame
data <- data.frame(income, happiness)

# Visualize the relationship
ggplot(data, aes(x = income, y = happiness)) +
  geom_point() +
  labs(x = "Income", y = "Happiness") +
  theme_minimal()

# Fit linear regression model
linear_model <- lm(happiness ~ income, data = data)

# Fit Poisson regression model (GLM)
poisson_model <- glm(happiness ~ income, data = data, family = poisson)

# Compare model summaries
summary(linear_model)
summary(poisson_model)

# Plot observed data points and fitted lines
ggplot(data, aes(x = income, y = happiness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Fitted line from linear regression
  stat_smooth(method = "glm", method.args = list(family = poisson), se = FALSE, color = "blue") +  # Fitted line from Poisson regression
  labs(x = "Income", y = "Happiness") +
  theme_minimal()


# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Simulate data
study_hours <- rnorm(100, mean = 30, sd = 10)  # Simulating study hours data
pass_prob <- plogis(0.1 * (study_hours - 20))  # Simulating probability of passing based on study hours
pass <- rbinom(100, 1, pass_prob)  # Simulating pass or fail based on probability

# Create data frame
data <- data.frame(study_hours, pass)

# Visualize the relationship
ggplot(data, aes(x = study_hours, y = pass)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Study Hours", y = "Pass (1) / Fail (0)") +
  theme_minimal()

# Fit linear regression model
linear_model <- lm(pass ~ study_hours, data = data)

# Fit logistic regression model (GLM)
logistic_model <- glm(pass ~ study_hours, data = data, family = binomial)

# Compare model summaries
summary(linear_model)
summary(logistic_model)
