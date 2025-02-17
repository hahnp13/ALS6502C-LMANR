library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(glmmTMB)
library(performance)

# EXAMPLE FOR CONDUCTING A ONE-WAY ANOVA WITH NON-NORMAL DATA IN R ######################################################

data("InsectSprays") ### load InsectSprays dataset, available from base R  

# filter to just 4 treatments
d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F') %>% droplevels()

## plot out data
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1) 
hist(d$count)

## build linear model ####
## construct linear model to examine the effect of the different sprays on insect counts
lm1 <- glmmTMB(count~spray, data=d)

### Anova table ####
Anova(lm1, type=2)  ## car::Anova will print out an ANOVA table testing

### examine coefficients and model components ####
summary(lm1)

### check residuals ####
hist(resid(lm1)) ## residuals should be normally distributed
plot(resid(lm1)~fitted(lm1)) +  ## residuals should be evenly dispersed around 0 across the range of x's
abline(h=0)                               # funnel shapes or curvature is bad

qqPlot(resid(lm1))  ## residuals should line up pretty closely to the blue line
boxplot(resid(lm1) ~ d$spray)  ## variances should be homogeneous for each group

### calculate emmeans
emmeans(lm1, ~spray) ## note all the SE are the same and CL seem weird

## build log-normal model ####
## construct log-normal model to examine the effect of the different sprays on insect counts
lm2 <- glmmTMB(log(count+1)~spray, data=d)
Anova(lm2, type=2)  ## car::Anova will print out an ANOVA table testing

## check residuals
hist(resid(lm2)) ## residuals should be normally distributed, even for glm
plot(resid(lm2)~fitted(lm2)) +  ## residuals should be evenly dispersed around 0 across the range of x's
  abline(h=0)                               # funnel shapes or curvature is bad

qqPlot(resid(lm2))  ## residuals should line up pretty closely to the blue line
boxplot(resid(lm2) ~ d$spray)  ## variances should be homogeneous for each group

## calculate emmeans
emmeans(lm2, ~spray) ## note that now all means are back-transformed

## calculate back-transformed emmeans
emmeans(lm2, ~spray, type='response') ## note that now all means are back-transformed


## build poisson model ####
## construct a generalized linear model to examine the effect of the different sprays
glm1 <- glmmTMB(count~spray, data=d, family='poisson')  ## glm is a general function that conducts a generalized linear model
# must specify the 'family' (aka distribution). Default is 'gaussian' aka normal.  
# all the "calculations" are saved in an object we called 'glm1'

Anova(glm1, type=2)  ## car::Anova will print out an ANOVA table testing 
# the null hypothesis that all group means are equal
# type = 2 provides margin tests, which is usually better than the default Type I, especially for more complicated models
# for GLMs Anova returns a Wald test with a chi-sq value


summary(glm1)   ## summary() will provide the model coefficients (ie. the "guts" of the model)
# the coefficients allow you rebuild the means from the linear model equation y~u+Bi
# rebuilding the model from the coefficients is not super helpful and the p-values aren't very meaningful
# residual deviance should be about equal to df. More than twice as high is problematic


## check assumptions of model by examining residuals
hist(resid(glm1)) ## residuals should be normally distributed, but sometimes aren't for GLMs
plot(resid(glm1)~fitted(glm1)) +  ## residuals should be evenly dispersed around 0 across the range of x's
abline(h=0)                               # funnel shapes or curvature is bad

qqPlot(resid(glm1))  ## calls from car package, residuals should line up pretty closely to the blue line
# points that drift from line might be outliers
boxplot(resid(glm1) ~ d$spray)  ## variances should be homogeneous for each group

## diagnosing more complex GLMs can be very difficult. Residuals are often NOT NORMALLY DISTRIBUTED. We will return to this later...

emmeans(glm1, ~spray) ## emmeans::emmmeans will rebuild the model for you
# this code will print off the means, SE, and confidence intervals for each treatment group
# coefficients are on the log-scale (look at model)

emmeans(glm1, pairwise~spray, type='response')  ## adding 'pairwise' will conduct pairwise contrasts -- ie. compare each group mean to the others
# automatically adjusts p-values using the 'tukey' adjust. Can change this if you want using adjust=XX
# the type='response' will back-transform (ie. exponentiate) to the original scale   

## compare residuals for normal, log-transformed, and poisson models #### 
par(mfrow=c(1,3))
boxplot(resid(lm1) ~ d$spray) 
boxplot(resid(lm2) ~ d$spray) 
boxplot(resid(glm1) ~ d$spray) 

dev.off()

## compare means and SE for normal, log-transformed, and poisson models ####
emmeans(lm1, ~spray)
emmeans(lm2, ~spray, type='response')
emmeans(glm1, ~spray, type='response')

# EXAMPLE FOR NEGATIVE BINOMIAL ####
## build negative binomial model ####
### first check for overdisersion in poisson model ####
check_overdispersion(glm1)

### make nb model ####
glm2 <- glmmTMB(count~spray, data=d, family='nbinom2')  

Anova(glm2)
summary(glm2) ## can look at dispersion parameter

## check assumptions of model by examining residuals
hist(resid(glm2)) ## residuals should be normally distributed, but sometimes aren't for GLMs
plot(resid(glm2)~fitted(glm2)) +  ## residuals should be evenly dispersed around 0 across the range of x's
  abline(h=0)                               # funnel shapes or curvature is bad

qqPlot(resid(glm2))  ## calls from car package, residuals should line up pretty closely to the blue line
# points that drift from line might be outliers
boxplot(resid(glm2) ~ d$spray)  ## variances should be homogeneous for each group



### look at residuals using simulateResiduals from DHARMa package ####
library(DHARMa)
simulateResiduals(lm1, plot=T) ## linear model
simulateResiduals(lm2, plot=T) ## log-linear model
simulateResiduals(glm1, plot=T) ## poisson model
simulateResiduals(glm2, plot=T) ## NB model

hist(simulateResiduals(glm2))

### print means - nearly identical to Poisson in this case ####
emmeans(glm2, ~spray, type='response')

