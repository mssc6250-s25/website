# Load the packages
library(infer)
library(ggplot2)
library(dplyr)

# Set Random seed 
set.seed(69)

# Load and subset the buit-in gss data
data(gss)
random_indices <- sample(nrow(gss), 20)
sample_dataset<- gss[random_indices, ]

# Find the regression line with the entire population data
population_fit <- gss %>% 
  specify(hours ~ age) %>% 
  fit()

# Fit Linear regression model with bootstrapped data 
boot_fits <- sample_dataset %>%
  specify(hours ~ age) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  fit()

# Get a point estimate of the observed dataset
observed_fit <- sample_dataset %>% 
  specify(hours ~ age) %>% 
  fit()

# With Percentile Method  get a 95% confidence interval
ci<-boot_fits  %>%  
  get_ci(point_estimate = observed_fit,level = 0.95,type = "percentile")


# Pull out the slopes (age) from the tibble
age_coeff_estimates <- boot_fits %>%
  filter(term == "age")%>%
  select(-term)

# Visualise
ggplot(age_coeff_estimates , aes(x = estimate)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") + 
  geom_vline(xintercept = population_fit[[2,2]],
             color = "red",linetype = "dashed",size = 3)+
  geom_vline(xintercept = observed_fit[[2,2]],
             color = "blue",size = 3)+
  geom_vline(xintercept = c(ci[[1,2]],ci[[1,3]]),
             color = "green",size = 1)+
  theme_bw()
