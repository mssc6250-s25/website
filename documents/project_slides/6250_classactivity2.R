# Load required libraries
library(MASS)
library(mgcv)

# Load Boston Housing dataset
data(Boston)

# View the structure of the dataset
str(Boston)

# Fit GAM model
#gam_model <- gam(medv ~ s(lstat) + s(rm) + crim + zn + indus + chas + nox + age + dis + rad + tax + ptratio + black + medv, data = Boston)

gam_model <- gam(medv ~ s(lstat) + s(rm) + crim + zn + indus + chas + nox + age + dis + rad + tax + ptratio + black, data = Boston)

# Summary of the model
summary(gam_model)

# Partial Dependence Plots
par(mfrow=c(1,2))
plot(gam_model, select=1, se=TRUE, col="blue")  # Partial dependence plot for lstat
plot(gam_model, select=2, se=TRUE, col="blue")  # Partial dependence plot for rm
par(mfrow=c(1,1))


# Component-Wise Partial Dependence Plots
plot.gam(gam_model, select=TRUE)

# Residuals vs. Fitted Values Plot
plot(gam_model, residuals=TRUE, col="blue", pch=16, main="Residuals vs Fitted Values")

# Smooth Terms Plot
plot(gam_model, shade=TRUE, col="lightblue", main="Smooth Terms Plot")
