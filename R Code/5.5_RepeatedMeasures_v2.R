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

#### chick as a random (block) effect ####
cw1 <- glmmTMB(weight ~ Time * Diet + (1|Chick), data=ChickWeight)

summary(cw1) 
Anova(cw1) ## Anova() from car package. Diet p-value is low, but note it uses Wald Chi-sq test.
hist(residuals(cw1)) ## resids from first model
plot(residuals(cw1)~fitted(cw1)) ## resids from first model

cw1_resid <- simulateResiduals(cw1) ## simulated resids doesn't seem to pick up corr structure
cw1_resid <- recalculateResiduals(cw1_resid, group=ChickWeight$Time)
testTemporalAutocorrelation(cw1_resid, time=unique(ChickWeight$Time), plot=T)


## Model incorperating autoregressive covariance structure ####
cw1ar <- glmmTMB(weight ~ Time * Diet + ar1(0 + as.factor(Time)|Chick), data=ChickWeight)

summary(cw1ar)
Anova(cw1ar)
hist(residuals(cw1ar)) ## hist looks ok
plot(residuals(cw1ar)~fitted(cw1ar)) ## resids from model w autoregressive covariance structure 


### check autocorr in residuals
cw1ar_resid <- simulateResiduals(cw1ar) ## need to group simulated residuals to check for autocorr
cw1ar_resid <- recalculateResiduals(cw1ar_resid, group=ChickWeight$Time)
testTemporalAutocorrelation(cw1ar_resid, time=unique(ChickWeight$Time))

## try toeplitz structure ####
cw1toep <- glmmTMB(weight ~ Time * Diet + toep(0 + as.factor(Time)|Chick), data=ChickWeight, dispformula = ~0)
## warnings are not a problem in this case
summary(cw1toep)

plot(residuals(cw1toep)~fitted(cw1toep)) ## resids a little wonky, can check simulated residuals 


cw1toep_resid <- simulateResiduals(cw1toep) 
cw1toep_resid <- recalculateResiduals(cw1toep_resid, group=ChickWeight$Time)
testTemporalAutocorrelation(cw1toep_resid, time=unique(ChickWeight$Time))

## try unstructured structure
cw1un <- glmmTMB(weight ~ Time * Diet + us(0 + as.factor(Time)|Chick), data=ChickWeight, dispformula = ~0)
summary(cw1un)

cw1un_resid <- simulateResiduals(cw1un) 
cw1un_resid <- recalculateResiduals(cw1un_resid, group=ChickWeight$Time)
testTemporalAutocorrelation(cw1un_resid, time=unique(ChickWeight$Time))

#### compare AIC for models including more complex toep structure
anova(cw1,cw1ar,cw1toep,cw1un) # toep is best; ar1 is probably fine (way better than no autocorrelation) unstructred also ok (but complex)


#### compare emmeans
emmeans(cw1, pairwise~Diet, at=list(Time=20)) # Yes, 4 of 6 differ at time 20
emmeans(cw1ar, pairwise~Diet, at=list(Time=20)) # Yes, 4 of 6 differ at time 20
emmeans(cw1toep, pairwise~Diet, at=list(Time=20)) # means are low; probably ar1 is best
emmeans(cw1un, pairwise~Diet, at=list(Time=20)) # means are low; probably ar1 is best


#### Add quadratic term and autoregressive covariance structure ####
cw1arq <- glmmTMB(weight ~ Time * Diet + I(Time^2) + ar1(0 + as.factor(Time)|Chick), data=ChickWeight)
cw1arq2 <- glmmTMB(weight ~ Time * Diet + I(Time^2) * Diet + ar1(0 + as.factor(Time)|Chick), data=ChickWeight)

summary(cw1arq)
Anova(cw1arq)
hist(residuals(cw1arq)) ## hist looks ok
plot(residuals(cw1arq)~fitted(cw1arq)) ## resids from model w/o autoregressive covariance structure 


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
head(ChickWeight)
cw_wide <- ChickWeight %>% group_by(Chick,Diet) %>% 
  pivot_wider(names_from = Time, values_from = weight, names_prefix = "Time")
?pivot_wider

ggpairs(cw_wide[5:12])

