# Remove objects from the workspace
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Set work dictionary
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load any necessary packages
lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

# Load data
data(child)
head(child)

# Select variables
# Covariables: mother's age and infant's gender
# Create a survival object using the Surv() function
child_surv <- with(child, Surv(enter, exit, event))
# Fit the Cox model
cox <- coxph(child_surv ~ m.age + sex, data = child)

# Summarize the model
summary(cox)

# Test the model, check how do all the variables contribute to the model
drop1(cox, test = "Chisq")
# Obtain the results for the model
stargazer(head(child))
stargazer(cox)

# Obtain hazard ratios
hrsex <- exp(-0.082215)

# Fit the survival curve
cox_fit <- survfit(cox)
# Plot the survival curve
pdf("Survial curve.pdf")
autoplot(cox_fit)
dev.off()

# Adding an interaction term
# Fit the model
cox.int <- coxph(child_surv ~ m.age*sex, data = child)
summary(cox.int)

# Test the model
drop1(cox.int, test = "Chisq")
stargazer(cox.int)














