#### R Challenge 10 

# Load libraries
# remember to install libraries if you have not used them before!
library(tidyverse) # includes ggplot2, for data visualisation. dplyr, for data manipulation.
library(survival)  # core survival analysis function
library(survminer) # recommended for visualizing survival curves
library(ggsurvfit) # alternative package for plotting time-to-event endpoints

# Load and investigate Cockroach dataset 
dat <- read.csv("R Challenges/roaches_insecticides.csv")

dim(dat) # returns the dimensions of the data frame
head(dat) # returns the first few rows from the data
str(dat)

# Part 1 ----
## Survival of Cockroaches to three insecticide applications ##

# 1. Create a survival object using Surv()
# hint: pay attention to your variable names! Surv([time variable], [event variable])

# 2a. Create KM survival curves using the function survfit()
# 2b. Create a plot using ggsurvplot()
# Does the application of insecticide result in increased mortality?

# Part 2 ----
# Investigate the survival of Cockroaches to three insecticide applications (A,B,C)
# Complete Steps 3-5 to answer the following questions:
# Q1: Are there differences in survival probability among the application groups?
# Q2: Does weight of the insect influence their survival probability?
# Hint: Because we want to know if the different application impact the hazard (and not if they are resulting in increased hazard compared to the control), we can remove the control group from our dataset:
dat2 <- dat %>% filter(group != "Control")

# 3. Fit a Cox proportional hazards regression model.
# Hint: Specify the model to include "weight" and "group", as well as the interaction between them.
# Which terms are important in the model?
# use anova() to help you select the most appropriate model.

# 4. Check model assumptions
# hint: Test the PH assumptions using cox.zph()
# If your final model includes weight, you also need to check for linearity.

# 5. Interpretation
# How do the hazards of the different groups compare? Try to put the results into words.
# Are groups B and C different? Hint: you need to change the reference group.

# Part 3 ----
# 6. Visualization
# Adjusted Survival Curves
# you can use the ggadjustedcurves() function from the survminer package:







# Optional: compare adjusted survival curves to KM survival curves
# Hint: remove control from the KM surival object
# load library(patchwork) for easy assembly of plots
library(patchwork)


