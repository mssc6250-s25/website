# Load necessary libraries
library(mgcv)
library(ggplot2)

# Simulate data with a non-linear relationship
set.seed(123)
n <- 500
x <- seq(0, 10, length.out = n)
y <- sin(x) + rnorm(n, sd = 0.2)

# Create data frame
data <- data.frame(x, y)

# Fit a GAM
gam_model <- gam(y ~ s(x), data = data)

# Fit a GLM
glm_model <- lm(y ~ x, data = data)

# Summary of the models
summary(gam_model)
summary(glm_model)

# Visualize the relationship between x and y
new_x <- seq(0, 10, length.out = 1000)
predicted_gam <- predict(gam_model, newdata = data.frame(x = new_x), type = "response")
predicted_glm <- predict(glm_model, newdata = data.frame(x = new_x))

# Plot the results
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "blue", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", size = 1) +
  labs(x = "x", y = "y", title = "Comparison of GAM and GLM") +
  theme_minimal()

