## LOAD AND PROCESSES DATA
library(tidyverse)   ### load tidyverse
library(car)         ### load car package, which is helpful for analyzing linear models
library(emmeans)     ### load emmeans package, which is helpful for getting means from linear models
library(viridis)     ### load viridis package for adding colors to plots
library(glmmTMB)

# EXAMPLE FOR CONDUCTING A ONE-WAY ANOVA IN R #################################

## load data ####
data("InsectSprays") ### load InsectSprays dataset, available from base R  

# filter to just 4 treatments
d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F') %>% droplevels()

## plot out data ####
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1) 
                                                    ## need to suppress outliers if you jitter plot points

hist(d$count) ## histogram of count data. Can we use to check assumption of normality?

## construct linear model to examine the effect of the different sprays on insect counts ####
## for a categorical variable (spray with four levels), we are interested in comparing group means
lm2 <- glmmTMB(count~spray , data=d)  ## lm is a general function that conducts a linear model
                                      # all the "calculations" are saved in an object we called 'lm2'


## Anova table ####
Anova(lm2, type=2)  ## car::Anova will print out an ANOVA table testing 
                     # the null hypothesis that all group means are equal
                     # type = 2 provides Type II sums of squares, which is usually the best way to go. Type III is also an option, but more complicated.
                     # other functions (anova, aov, etc.) will provide similar ANOVA tables, but the Anova() is more flexible

## coefficients and model components ####
summary(lm2)   ## summary() will provide the model coefficients (ie. the "guts" of the model)
                # the coefficients allow you rebuild the means from the linear model equation y~u+Bi
                # rebuilding the model from the coefficients is not super helpful and the p-values aren't very meaningful

tidy(lm2, conf.int=TRUE) # tidy() to examine coefficients
glance(lm2) # glance at model fit stats

## examine group means ####
emmeans(lm2, ~spray) ## emmeans::emmmeans will rebuild the model for you
                      # this code will print off the means, SE, and confidence intervals for each treatment group

## pairwise comparisons of group means ####
emmeans(lm2, pairwise~spray)  ## adding 'pairwise' will conduct pairwise contrasts -- ie. compare each group mean to the others
                               # automatically adjusts p-values using the 'tukey' adjust. Can change this if you want using adjust=XX

## check assumptions of model by examining residuals ####
hist(resid(lm2)) ## residuals should be normally distributed
plot(resid(lm2)~fitted(lm2))  ## residuals should be evenly dispersed around 0 across the range of x's
abline(h=0)                             # funnel shapes or curvature is bad

qqPlot(resid(lm2))  ## calls from car package, residuals should line up pretty closely to the blue line
                        # points that drift from line might be outliers
boxplot(resid(lm2) ~ d$spray)  ## variances should be homogeneous for each group

          ## problems with residuals indicate assumptions of the linear model are violated and may cause problems with coefficients and p-values
          ## transforming the data or using a different type of model may help (we will return to this example later in the course to improve it)
          ## assumptions can be slightly violated without causing problems, for example this model is seems passable but could be better.

## Plot data with means ####
## Plot data and add means plus SE from your emmeans. Can change colors, if you'd like.
em1 <- emmeans(lm1, ~spray) %>% as.data.frame()

ggplot() + 
  geom_boxplot(data=d, aes(x=spray,y=count), outlier.shape = NA) + 
  geom_jitter(data=d, aes(x=spray,y=count), height=0,width=.1, size=2) + 
  geom_pointrange(data=em1, aes(x=spray, y=emmean, ymin=emmean-SE, ymax=emmean+SE),
                color="red", size=1.5, linewidth=2) + ##geom_errorbar() call also be used to draw error bars
  theme_bw(base_size = 14)

