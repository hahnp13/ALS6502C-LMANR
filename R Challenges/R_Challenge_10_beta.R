### R Challenge 10 -- beta ###

library(glmmTMB)
library(emmeans)
library(car)
library(DHARMa)
library(tidyverse)
library(performance)

#################################################################################################
#### 1. problem set w/ real data (Rivkin et al. 2018 Am J Bot) #####
#### herbivory data collected on 43 populations of an aquatic plant across a latitudinal gradient in Canada
#### At each population, many plants (~5-15) were examined for herbivory damage
#### Some additional covariates were recorded, such as Competition around the plant (1-3 from less to more) and plant height (cm)

### Q: Does herbivory increase towards the equator?

# Step 0: Read in data and explore ####
d1 <-read_csv("R Challenges/ajb21098-sup-0002-appendixs2.csv")
head(d1)
hist(d1$LeafDamage)
summary(d1$LeafDamage)

## removes 0's and 1's for plotting (see Smithson & Verkuilen 2006 or Douma & Weedon 2018)
d1$LeafDamage1 <- (d1$LeafDamage*(length(d1$LeafDamage)-1)+.5)/length(d1$LeafDamage)

ggplot(d1 , aes(x=Latitude, y=LeafDamage1)) + geom_point() +
  geom_smooth(method='glm', method.args=list(family="ordbeta"))

# Step 1: Fit model ####

# Step 2: Check residuals ####

# Step 3: Check summary, random effects, etc. ####

# Step 4: Check Anova ####

# Step 5/6: Check emmeans / trends -- may not be necassary depending on terms in your model ####

# Step 7: Check R2 #### 