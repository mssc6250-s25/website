# Import the packages
library(infer)
library(ggplot2)
library(dplyr)

# Set Random seed 
set.seed(6520)

# Load the built in gss dataset
data(gss)

# Take a look at the population dataset
dplyr::glimpse(gss)


# Find the population mean 
population_mean <- mean(gss$hours)

# Randomly choose 20 row indices from the original dataframe
random_indices <- sample(nrow(gss), 20)

# Subset the original dataframe using the randomly chosen indices
sample_dataset<- gss[random_indices, ]

# Take a look at the sample dataset
dplyr::glimpse(sample_dataset)



# Finding the mean with Bootstrapping
boot_dist <- sample_dataset %>%
  specify(response = hours) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

# Visualize bootstrap distribution
boot_dist %>%
  visualize()+
  theme_bw()
  
# Form the confidence interval
ci <- infer::get_ci(
  boot_dist, level = .95)


# Compute the sample mean
obs_mean <- sample_dataset %>%
  specify(response = hours) %>%
  calculate(stat = "mean")


boot_dist %>%
  visualize() +
  shade_confidence_interval(endpoints = ci)+
  geom_vline(xintercept = population_mean,
             color = "red",linetype = "dashed",size = 3)+
  geom_vline(xintercept = obs_mean[[1]],
             color = "blue",size = 3)+
  theme_bw()

