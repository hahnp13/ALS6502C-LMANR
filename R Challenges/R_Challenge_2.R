####################################################################################
##### R CHALLENGE 2 - Categorical (ANOVA) ##########################################

library(tidyverse)
library(car)
library(emmeans)
library(glmmTMB)
library(easystats)

## have a look at the dataset below. Baby chickens were fed different diets and they were weighed after 10 days. 
## variable 'weight' is the weight of a baby chicken (g); 'feed' is the type of type of diet the chicken was fed

d1 <- chickwts
head(d1)

summary(d1)
ggplot(d1, aes(x= feed, y=weight))+ geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=0.1)

# 1. Construct a linear model to analyze the data. ####


# 2. Are the assumptions met? ####


# 3. Is there evidence at least one mean is different than another? ####


# 4. How much variation in the data does the model explain? ####


# 5. The feed 'casein' is the standard chicken diet. What types of feed have the highest means?  ####


# 6. Which diets are significantly different than 'casein'? By how much are they worse?


# 7. Make a nice looking figure. show all the data. #7a Try making a boxplot or violin with jittered data points and then overlay the means w/ SE.
# Try making a boxplot with jittered points and then overlay the mean +/- SE in a large dot of a different color.
# Try changing the color of each feed treatment. Customize the colors, themes, etc. to make it look nice and readable.

