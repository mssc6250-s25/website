##### GLM with Smarket data #####
library(ISLR2)

names(Smarket)
dim(Smarket)

summary(Smarket)

#cor(Smarket) #Error in cor(Smarket) : 'x' must be numeric
cor ( Smarket [ , -9])

attach(Smarket)
plot(Volume)

Smarket$Direction <- as.factor(Smarket$Direction)

glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket,
  family = binomial
)

summary (glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]

contrasts(Direction)
glm.pred <- rep("Down", 1250)
glm.pred [glm.probs > .5] = "Up"

table(glm.pred, Direction)
(507 + 145) / 1250
mean (glm.pred == Direction)

train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[! train ,]
dim (Smarket.2005)

Direction.2005 <- Direction[! train]

glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket,
  family = binomial,
  subset = train
)

# Load required library
library(pROC) # For ROC curve
# Plot Histogram of Predicted Probabilities
hist(glm.probs, breaks = 20, col = "lightblue", main = "Histogram of Predicted Probabilities", xlab = "Predicted Probabilities")

# ROC Curve
roc_curve <- roc(Smarket$Direction, glm.probs)
plot(roc_curve, main="ROC Curve", col="blue")
abline(a=0, b=1, lty=2, col="red") # Diagonal reference line

##### GAM with mtcars data #####

library(ggplot2)
data(mtcars)
head(mtcars)

# Fit a GAM to model the relationship between mpg and hp
gam_model <- gam(mpg ~ s(hp), data = mtcars)

# Summary of the GAM
summary(gam_model)

# Generate the random hp dataset for predictions
new_data <- data.frame(hp = seq(min(mtcars$hp), max(mtcars$hp), 
                                length.out = 100))
predictions <- predict(gam_model, newdata = new_data, type = "response", 
                       se.fit = TRUE)

# Plot the data and the GAM fit
ggplot() +
  geom_point(data = mtcars, aes(x = hp, y = mpg)) +
  geom_line(data = data.frame(hp = new_data$hp, mpg = predictions$fit), 
            aes(x = hp, y = mpg), color = "blue") +
  geom_ribbon(data = data.frame(hp = new_data$hp, fit = predictions$fit, 
                                se = predictions$se.fit), aes(x = hp, 
                                                              ymin = fit - 1.96 * se, 
                                                              ymax = fit + 1.96 * se), alpha = 0.3) +
  labs(title = "Generalized Additive Model (GAM) Fit for mpg vs. hp", 
       x = "Horsepower", y = "Miles per Gallon") +
  theme_minimal()

##### GAM with Boston data #####
# Load required libraries
library(MASS)
library(mgcv)

# Load Boston Housing dataset
data(Boston)

# View the structure of the dataset
str(Boston)

# Fit GAM model
gam_model <- gam(medv ~ s(lstat) + s(rm) + crim + zn + indus + chas + nox + age + dis + rad + tax + ptratio + lstat, data = Boston)

# Summary of the model
summary(gam_model)

# Partial Dependence Plots
par(mfrow=c(1,2))
plot(gam_model, select=1, se=TRUE, col="blue", main="Partial Dependence Plot for lstat")  # Partial dependence plot for lstat
plot(gam_model, select=2, se=TRUE, col="blue", main="Partial Dependence Plot for rm")  # Partial dependence plot for rm
par(mfrow=c(1,1))

# Component-Wise Partial Dependence Plots
plot.gam(gam_model, select=TRUE)

# Residuals vs. Fitted Values Plot
plot(gam_model, residuals=TRUE, col="blue", pch=16, main="Residuals vs Fitted Values")

# Smooth Terms Plot
plot(gam_model, shade=TRUE, col="lightblue", main="Smooth Terms Plot")