############################################
#### CHALLENGE 9 

library(glmmTMB)
library(emmeans)
library(car)
library(RVAideMemoire)
library(DHARMa)
library(MuMIn)
library(tidyverse)
library(performance)

## read in and prepare data
## data from https://doi.org/10.5061/dryad.6djh9w0zj
exp16 <-read_csv("Maron_etal_2020_JEcol_Data.csv")
exp16$Year <- factor(exp16$Year, levels=c("Seedlings","Adults_Y3"))  ## reorder factor levels
levels(exp16$Year) <- c("Seedlings","Adults")

## filter data to exclude certain treatments
dat1 <- exp16 %>% filter(Rodents=='+',Year=='Adults') %>% select(SiteCode,Competition,Species,Fecundity,numb)

## Use dat1 for the Challenge
head(dat1)
tail(dat1)

## examine histograms
hist(dat1$numb) ## response variable
hist(dat1$Fecundity)  ## predictor variable
hist(log10(dat1$Fecundity))  ## log10-transformed predictor variable

dat1$Fecundity_log10 <- log10(dat1$Fecundity) ## transform predictor

## QUESTIONS
## 1. Do more fecund plant species produce more seedlings, and is this affected by competition?
##   Follow all the "Steps for constructing and interpreting a GLMM"

mod1 <- glmmTMB(numb ~ Fecundity_log10 * Competition + (1|SiteCode/Competition), family=nbinom2, data=dat1)

r2(mod1)
r.squaredGLMM(mod1)

ggplot(dat1, aes(x=Fecundity_log10, y=numb, color=Competition))+
  geom_point()+
  geom_smooth(method="lm")
  


dat1.plot<- ggplot() +
  geom_point(data=dat1.plot, aes(x=Fecundity_log10, y=numb, color=Species), 
             position=position_jitterdodge(jitter.width=.15, dodge.width = .75), alpha=.5) +
  scale_color_viridis(discrete = T, direction = -1, end=.8) +
  ylab("numb") + theme_bw(base_size = 20)
