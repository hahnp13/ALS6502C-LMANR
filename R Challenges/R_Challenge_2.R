####################################################################################
##### R CHALLENGE 2 - Categorical (ANOVA) ##########################################

library(tidyverse)
library(car)
library(emmeans)
library(glmmTMB)

## have a look at the dataset below. Baby chickens were fed different diets and they were weighed after 10 days. 
## variable 'weight' is the weight of a baby chicken (g); 'feed' is the type of type of diet the chicken was fed

d1 <- chickwts
head(d1)

# 1. Construct a linear model to analyze the data. Is there evidence at least one mean is different than another?

# 2. How much variation in the data does the model explain?

# 3. The feed 'casein' is the standard chicken diet. What types of feed are significantly worse than 'casein'. By how much are they worse?

# 4. Are the assumptions met?

# 5. Make a nice looking figure. show all the data. #5a Try making a boxplot or violin with jittered data points and then overlay the means w/ SE.
# Try making a boxplot with jittered points and then overlay the mean +/- SE in a large dot of a different color.
# Try changing the color of each feed treatment. Customize the colors, themes, etc. to make it look nice and readable.

