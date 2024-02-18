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

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
write.csv(climateSupport, "climateSupport.csv")

# Question 1
data <- climateSupport
# Transfer choice into binary variable
data$choice <- ifelse(data$choice == "Supported", 1, 0)

# Convert countries and sanctions columns into factors
data$countries <- factor(data$countries, levels = c("20 of 192", "80 of 192", "160 of 192"), ordered = FALSE)
data$sanctions <- factor(data$sanctions, levels = c("None", "5%", "15%", "20%"), ordered = FALSE)
# Change reference level
data$countries <- relevel(data$countries, ref = "20 of 192")
data$sanctions <- relevel(data$sanctions, ref = "None")
# Run the glm model
glm_model <- glm(choice ~ countries + sanctions, family = binomial(link = "logit"), data = data)
summary(glm_model)
stargazer(glm_model)
 
# Run glm model with interaction
glm_model_interaction <- glm(choice ~ countries*sanctions, family = binomial(link = "logit"), data = data)
summary(glm_model_interaction)
stargazer(glm_model_interaction)
