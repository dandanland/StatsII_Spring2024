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

lapply(c("nnet", "MASS", "stargazer", "xtable", "ggplot2", "AER", "pscl"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# (1)
# Convert GDPWdiff to a factor
gdp_data$GDPWdiff <- as.factor(ifelse(gdp_data$GDPWdiff > 0, "positive",
                                      ifelse(gdp_data$GDPWdiff < 0, "negative", "no change")))
# Set the reference level
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no change")

# Run an unordered multinomial logit model
multi_unordered <- multinom(GDPWdiff ~ REG + OIL, data = gdp_data, reflevel = "no change")
multi_unordered
# Exponentiate the coefficients
exp(coef(multi_unordered))
stargazer(exp(coef(multi_unordered)))
stargazer(multi_unordered)

# (2)
# Convert GDPWdiff to an ordered factor
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff, levels = c("negative", "no change", "positive"), ordered = TRUE)

# Run an ordered multinomial logit model
multi_ordered <- polr(GDPWdiff ~ REG + OIL, data = gdp_data, Hess = TRUE)

# Calculate a  p value
ctable <- coef(summary(multi_ordered))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
stargazer((ctable <- cbind(ctable, "p value" = p)))

# Get odds ratios and CIS
exp_coefs <- exp(cbind(OR = coef(multi_ordered), confint(multi_ordered)))
stargazer(exp_coefs)
stargazer(multi_ordered)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# (a)
# Check the structure of the data
head(mexico_elections)

hist(mexico_elections$PAN.visits.06)
ggplot(mexico_elections, aes(x = PAN.visits.06, y = factor(competitive.district), color = marginality.06, shape = factor(PAN.governor.06))) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Number of PAN Visits in 2006", y = "Competitive District", color = "Marginality", shape = "PAN Governor in 2006") +
  ggtitle("Visualization of PAN Visits by District Characteristics")

# Fit a poisson regression model
model_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, family = poisson, data = mexico_elections)
summary(model_poisson)

# Check equal variance assuption
dispersiontest(model_poisson)

# Extract p-value and test statistic
p_value <- coef(summary(model_poisson))[, 4]
test_statistic <- coef(summary(model_poisson))[, 3]
stargazer(cbind(test_statistic, p_value))
stargazer(model_poisson)

# (b)

# (c)
# Create a new dataframe for hypothetical district
new_data <- data.frame(competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1)
# Predict the mean number of visits from the winning PAN presidential candidate
pred_visits <- predict(model_poisson, newdata = new_data, type = "response")
exp(pred_visits)
print(pred_visits)


















