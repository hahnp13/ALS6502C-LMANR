# EXAMPLE OF LINEAR REGRESSION IN R #############################################
library(tidyverse)
library(car)
library(glmmTMB)
library(broom.mixed)
library(MuMIn)

set.seed(21)
temp <- round(runif(20,12,30), 2)            
mass <- round(rnorm(20,5*temp,25), 2)
r1 <- as.data.frame(cbind(temp,mass)) ### run lines 7-10 to generate some fake data

head(r1) ## temp is rearing temperature (C), mass is the mass of adults (mg)

## Step 0. Examine plot data ####
summary(r1)
hist(r1$mass)
ggplot(r1, aes(x=temp, y=mass))+geom_point(size=2)+theme_bw(base_size=20)

# Step 1. build model ####
## construct a linear model to estimate the average adult mass per degree C of temperature increase
## for a continuous variable (temp in degree C), we are interested in estimating the slope between age and circumference

lm1 <- glmmTMB(mass~temp, data=r1)


# STEP 2. check assumptions of model by examining residuals ####
hist(resid(lm1)) ## residuals should be normally distributed
plot(resid(lm1)~fitted(lm1))  ## residuals should be evenly dispersed around 0 across the range of x's
abline(h=0)                             # funnel shapes or curvature is bad

qqPlot(resid(lm1))  ## calls from car package, residuals should line up pretty closely to the blue line
## points that drift from line may be outliers

## problems with residuals indicate assumptions of the linear model are violated and may cause problems with coefficients and p-values
## transforming the data may help
## assumptions can be slightly violated without causing problems, for example this model is seems decent


# STEP 3. Look at model coefficients ####
summary(lm1)        ## summary() will provide the model coefficients (ie. the "guts" of the model)
# the coefficients allow you rebuild the means from the linear model equation y~B0+B1*X
# for continuous variables these coefficients and p-values are very useful (unlike for categorical ANOVA)
# don't really need to look at ANOVA table or use emmeans for this type of analysis, everything of interest is in summary

### other ways to get model coefficients ####
fixef(lm1)   ## look at model coefficients

### the broom.mixed package has a few useful functions for printing off model components
tidy(lm1, conf.int=TRUE)    ## look at model coefficients, allows a way to extract coefficients for using in other things.
# tidy() function is usually more useful than summary() because it prints off the coefficients in a more convient way.

glance(lm1) ## glance() can be used to look at other components of the model
augment(lm1) ## augment will print off the fitted vales and residuals

r2(lm1) ## calculate R2 (use R2, not adj. R2)


# STEP 4. Examine Anova Table to check significance ####
Anova(lm1, type=2)  ## produces an ANOVA table
# tests the null hypothesis that the slope is different than zero
# not super useful for regressions, because we see similar info in the summary() but can look at
# this will be much for helpful for categorical variables and interactions later in the course


## fancy-ish plot ####
ggplot(r1, aes(x=temp, y=mass))+
  geom_point(size=3,color='blue')+
  geom_smooth(method='lm')+
  theme_bw(base_size = 16)

