# R CHALLENGE 1 - REGRESSON #############################################################################
library(tidyverse)
library(car)
library(glmmTMB)
library(easystats)

# ---------------------------------------------------------
# Background Information #####
## Healthy orange trees typically produce fruit at 100 cm in circumference
## A homeowner calls and says their orange tree is 3 years old (1095 days), but isn't fruiting. They didn't measure it.
## They also said their are some yellow spots on the leaves. 
## Lucky for you, you have a previous data set you can use to answer their questions.


# Step 0: run the code below and have a look at the dataset orange ####
data("Orange")  ## load Orange dataset from base R
head(Orange)    ## measurements of circumference on five trees at 7 time points

## hint: plot out data to examine (replace ? with variable name)
ggplot(Orange, aes(x= ???, y= ???))+
  geom_point()+
  theme_bw(base_size = 16)

# ---------------------------------------------------------
# Answer the following questions: ####
## 1: Build a linear model that will allow you answer the question of whether their 3 year old tree should be fruits ####
## that is, what should the circumference of the 1095 day old tree be?

## 2. Are the model assumptions met?


## 3. What circumference should their tree be, on average? (provide an answer to the nearest centimeter)


## 4. Should their tree be fruiting by now?  


## 5. What advice would you give the grower? 


## 6. Make a nice figure. Change themes, etc.

