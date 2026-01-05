## LOAD AND PROCESSES DATA
library(tidyverse)   ### load tidyverse
library(car)         ### load car package, which is helpful for analyzing linear models
library(viridis)     ### load viridis package for adding colors to plots
library(glmmTMB)
library(easystats)

options(modelbased_backend = "emmeans") ## set easystats backend to "emmeans" by default

# EXAMPLE FOR CONDUCTING A ONE-WAY ANOVA IN R #################################

## load data ####
data("InsectSprays") ### load InsectSprays dataset, available from base R  

# filter to just 4 treatments
d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F') %>% droplevels()

# STEP 0. explore and plot out data ####
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1) 
                                                    ## need to suppress outliers if you jitter plot points

hist(d$count) ## histogram of count data. Can we use to check assumption of normality?

# STEP 1. construct linear model to examine the effect of the different sprays on insect counts ####
## for a categorical variable (spray with four levels), we are interested in comparing group means
lm2 <- glmmTMB(count~spray , data=d)  ## glmmTMB is a general function that conducts a linear model
                                      # all the "calculations" are saved in an object we called 'lm2'


# STEP 2. check assumptions of model by examining residuals ####
check_model(lm2)

hist(resid(lm2)) ## residuals should be normally distributed

## look ok-ish but there are some problems with residuals indicating assumptions of the linear model are violated and may cause problems with coefficients and p-values
## transforming the data or using a different type of model may help (we will return to this example later in the course to improve it)
## assumptions can be slightly violated without causing problems, for example this model is seems passable but could be better.


# STEP 3. examine coefficients and model components ####
summary(lm2)   ## summary() will provide the model coefficients (ie. the "guts" of the model)
# the coefficients allow you rebuild the means from the linear model equation y~u+Bi
# rebuilding the model from the coefficients is not super helpful with multiple categories/groups and the p-values aren't very meaningful

model_parameters(lm2) # examine coefficents from easystats package


# STEP 4. Check significane with Anova table ####
Anova(lm2)  ## car::Anova will print out an ANOVA table testing 
                     # the null hypothesis that all group means are equal
                     # type = 2 (default) provides Type II sums of squares, which is usually the best way to go. See course manual for notes about Type II vs III
                     # other functions (anova, aov, etc.) will provide similar ANOVA tables, but the Anova() is more flexible

# STEP 5. examine group means ####
estimate_means(lm2, ~spray) ## calls emmeans 
                                               # this code will print off the means, SE, and confidence intervals for each treatment group

emmeans::emmeans(lm2, ~spray) ## can call emmeans directly from package

# STEP pairwise comparisons of group means ####
estimate_contrasts(lm2)

emmeans(lm2, pairwise~spray)  ## adding 'pairwise' will conduct pairwise contrasts -- ie. compare each group mean to the others
                               # automatically adjusts p-values using the 'tukey' adjust. Can change this if you want using adjust=XX

## Plot data with means ####
## Plot data and add means plus SE from your emmeans. Can change colors, if you'd like.
em1 <- estimate_means(lm2, ~spray) %>% as_tibble()

ggplot() + 
  geom_boxplot(data=d, aes(x=spray,y=count), outlier.shape = NA) + 
  geom_jitter(data=d, aes(x=spray,y=count), height=0,width=.1, size=2) + 
  geom_pointrange(data=em1, aes(x=spray, y=Mean, ymin=CI_low, ymax=CI_high),
                color="purple", size=1.25, linewidth=2) + ##geom_errorbar() call also be used to draw error bars
  theme_bw(base_size = 14)


## re-run using lm ####
lm2a <- lm(count~spray , data=d)
Anova(lm2a) # note F-value instead of X2 and you get Sums of Squares.
summary(lm2a) # almost identical to lm()
hist(resid(lm2a))
