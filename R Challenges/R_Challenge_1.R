# R CHALLENGE 1 - REGRESSON #############################################################################
library(tidyverse)
library(car)
library(glmmTMB)
library(easystats)

# ---------------------------------------------------------
# Step 1: Background Information #####
## Healthy orange trees typically produce fruit at 100 cm in circumference
## A homeowner calls and says their orange tree is 3 years old (1095 days), but isn't fruiting. They didn't measure it.
## They also said their are some yellow spots on the leaves. 
## Lucky for you, you have a previous data set you can use to answer their questions.


# Step 2: run the code below and have a look at the dataset orange ####
data("Orange")  ## load Orange dataset from base R
head(Orange)    ## measurements of circumference on five trees at 7 time points

## hint: plot out data to examine (replace ? with variable name)
ggplot(Orange, aes(x= ???, y= ???))+
  geom_point()+
  theme_bw(base_size = 16)

# ---------------------------------------------------------
# Step 3: Build a linear model (and make plot) to answer the following questions: ####
## 1. What circumference should their tree be, on average? (provide an answer to the nearest centimeter)


## 2. Are the model assumptions met?


## 3. Should their tree be fruiting by now?  


## 4. What advice would you give the grower? 


## 5. Make a nice figure. Change themes, etc.

