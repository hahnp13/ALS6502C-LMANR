#### R CODE FOR 5.5_Repeated Measures
## LOAD AND PROCESSES DATA
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)
library(emmeans)
library(viridis)
library(MuMIn)
library(glmmTMB)
library(DHARMa)
library(performance)

data("ChickWeight")
ChickWeight$Diet <- as.factor(ChickWeight$Diet)
?ChickWeight

## plot out data
ggplot(data=ChickWeight)+
  geom_point(data=ChickWeight, aes(x=Time,y=weight, color=as.numeric(Chick)))+
  scale_color_viridis() +
  facet_wrap(~Diet)+
  geom_smooth(data=ChickWeight, method="lm",aes(x=Time,y=weight))+
  theme_bw(base_size = 16)

ggplot(data=ChickWeight)+
  geom_point(data=ChickWeight, aes(x=as.factor(Time),y=weight, color=as.numeric(Chick)))+
  scale_color_viridis() +
  facet_wrap(~Diet)+
  #geom_smooth(data=ChickWeight, method="lm",aes(x=Time,y=weight))+
  theme_bw(base_size = 16)

# chick as a random (block) effect ####
cw1 <- glmmTMB(weight ~ Time * Diet + (1|Chick), data=ChickWeight)

plot(residuals(cw1)~fitted(cw1)) ## resids a little wonky, can check simulated residuals 

summary(cw1) 
Anova(cw1) ## Anova() from car package. Diet p-value is low, but note it uses Wald Chi-sq test.

## check residuals using DHARMa package ####
simulateResiduals(cw1, plot=T) ## simulated residuals

# check autocorrelation with DHARMa
cw1_resid <- simulateResiduals(cw1) 
cw1_resid <- recalculateResiduals(cw1_resid, group=ChickWeight$Time)
testTemporalAutocorrelation(cw1_resid, time=unique(ChickWeight$Time), plot=T)

## check residuals with performance package ####
check_model(cw1) ## several diagnostic plots from performance package, can be used in conjunction with DHARMa::simulateResiduals 
check_autocorrelation(cw1)  ## basic test here, the DHARMa is a little more robust


# Model incorporating autoregressive covariance structure ####
cw1ar <- glmmTMB(weight ~ Time * Diet + ar1(0 + as.factor(Time)|Chick), data=ChickWeight)

summary(cw1ar)

plot(residuals(cw1ar)~fitted(cw1ar)) ## resids look good 

## check residuals 
simulateResiduals(cw1ar, plot=T) ## simulated residuals

## check autocorr in residuals
cw1ar_resid <- simulateResiduals(cw1ar) ## need to group simulated residuals to check for autocorr
cw1ar_resid <- recalculateResiduals(cw1ar_resid, group=ChickWeight$Time)
testTemporalAutocorrelation(cw1ar_resid, time=unique(ChickWeight$Time))

## check residuals with performance package ####
check_model(cw1ar) ## several diagnostic plots from performance package, can be used in conjunction with DHARMa::simulateResiduals 
check_autocorrelation(cw1ar)  ## basic test here, the DHARMa is a little more robust


## try toeplitz structure ####
cw1toep <- glmmTMB(weight ~ Time * Diet + toep(0 + as.factor(Time)|Chick), data=ChickWeight, dispformula = ~0)
## warnings are not really a problem in this case
summary(cw1toep)

# check residuals
plot(residuals(cw1toep)~fitted(cw1toep)) ## resids a little wonky, can check simulated residuals 
simulateResiduals(cw1toep, plot=T) ## simulated residuals looks wonky
check_model(cw1toep) ## several diagnostic plots from performance package, can be used in conjunction with DHARMa::simulateResiduals 

# check autocorrelation
cw1toep_resid <- simulateResiduals(cw1toep) 
cw1toep_resid <- recalculateResiduals(cw1toep_resid, group=ChickWeight$Time)
testTemporalAutocorrelation(cw1toep_resid, time=unique(ChickWeight$Time))

#### compare AIC for models including more complex toep structure
anova(cw1,cw1ar,cw1toep) # toep is best; ar1 is probably fine (way better than no autocorrelation) unstructred also ok (but complex)
compare_performance(cw1,cw1ar,cw1toep) # compare with performance package

#### compare emmeans
emmeans(cw1, pairwise~Diet, at=list(Time=20)) # Yes, 4 of 6 differ at time 20
emmeans(cw1ar, pairwise~Diet, at=list(Time=20)) # Yes, 4 of 6 differ at time 20
emmeans(cw1toep, pairwise~Diet, at=list(Time=20)) # means are low; probably ar1 is best


#### Add quadratic term and autoregressive covariance structure ####
cw1arq <- glmmTMB(weight ~ Time * Diet + I(Time^2) + ar1(0 + as.factor(Time)|Chick), data=ChickWeight)

summary(cw1arq)
Anova(cw1arq)
hist(residuals(cw1arq)) ## hist looks ok
plot(residuals(cw1arq)~fitted(cw1arq)) ## resids from model w/o autoregressive covariance structure 

plot(simulateResiduals(cw1arq))

#### compare AIC for models
AIC(cw1,cw1ar,cw1arq,cw1arq2)
BIC(cw1,cw1ar,cw1arq,cw1arq2)

## plot out data with quadratic 
ggplot(data=ChickWeight)+
  geom_point(data=ChickWeight, aes(x=Time,y=weight, color=as.numeric(Chick)))+
  scale_color_viridis() +
  facet_wrap(~Diet)+
  geom_smooth(data=ChickWeight,aes(x=Time,y=weight), method="lm", formula = y~x+I(x^2))+
  theme_bw(base_size = 16)

#################################
## Code for correlating different time points
library(GGally)
head(ChickWeight)
cw_wide <- ChickWeight %>% group_by(Chick,Diet) %>% 
  pivot_wider(names_from = Time, values_from = weight, names_prefix = "Time")
?pivot_wider

ggpairs(cw_wide[5:12])

