#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
# Set seed for reproducibility
set.seed(123)

# Generate Cauchy distributed data
data <- rcauchy(1000, location = 0, scale = 1)

# Sort the data 
sorted_data <- sort(data)

# Create empirical distribution of observed data
ECDF <- ecdf(data)

# Calculate the theoretical CDF values for the sorted data
theoretical_CDF <- pnorm(sorted_data, mean = 0, sd = 1)

# Calculate the D statistic
n <- length(data)
D <- max(c(abs((1:n)/n - theoretical_CDF), abs(theoretical_CDF - (0:(n-1))/n)))
D
# Calculate the p-value for the D statistic
p_value_calculate <- function(D) {
  # As the series is infinite, here choose k_max = 1000 to truncate it.
  k <- 1:1000
  sum_exp <- sum(exp(-((2*k - 1)^2 * pi^2) / (8 * D^2)))
  p_value <- (sqrt(2 * pi) / D) * sum_exp
  # Make sure p-value not exceed 1
  min(p_value, 1)
}

# Calculate the p-value for the computed D statistic
p_value <- p_value_calculate(D)
p_value
# Do KS test to double check 
# Compare the Cauchy distributed data against a normal distribution with mean and sd estimated from the data
results <- ks.test(data, "pnorm", mean = 0, sd = 1)
results

#####################
# Problem 2
#####################
# Set seed for reproducibility
set.seed(123)
# Create data
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Estimate OLS model using optim()
# Define the loss function as the sum of squared residuals
loss_function <- function(params, data) {
  with(data, sum((y - (params[1] + params[2] * x))^2))
}

# Initial parameter guesses
initial_params <- c(intercept = 0, slope = 1)
# Use BFGS algorithm via optim()
optim_results <- optim(
  par = initial_params,
  fn = loss_function,
  data = data,
  method = "BFGS",
  hessian = TRUE
)

# Convert optim results to tidy format
optim_coefs <- setNames(optim_results$par, c("(Intercept)", "x"))

# Estimate OLS model using lm()
lm_model <- lm(y ~ x, data=data)

# Compare results
summary(lm_model)$coefficients
optim_coefs

stargazer(lm_model,optim_coefs)



