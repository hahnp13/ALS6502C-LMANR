#### R CODE FOR 5.3_VarianceComponents and R squares
## LOAD AND PROCESSES DATA
library(tidyverse)
library(car)
library(lme4)
library(lmerTest)
library(emmeans)
library(patchwork)
library(MuMIn)

########################################################################################################
#### Basic R2 example #########################################################################

## make up some data as an example. Run lines 16-21 to make the dataset.
## measurements of body size on 5 insects at 8 sites, so 5 sub-replicates per site
set.seed(10)
fakedata <- data.frame(Site=factor(40), ID=factor(40), temp=double(40), size=double(40), stringsAsFactors = F)
fakedata$Site <- rep(1:8, each=5)
fakedata$ID <- rep(1:5, times=8)
fakedata$temp <- rep(c(10,18,12,15,8,11,10,16), each=5)
fakedata$size <- round(rnorm(40, (2*fakedata$temp), 8), 1)

head(fakedata)
hist(fakedata$size)


## make plot of fake data
ggplot(fakedata, aes(x=temp, y=size)) + geom_point() + geom_smooth(method="lm") + theme_bw(base_size=16)

### calculate R2 for linear model and linear mixed model
lm1 <- lm(size ~ temp , data=fakedata)
summary(lm1) # look at R2

lmm1 <- lmer(size ~ temp + (1|Site), data=fakedata)
summary(lmm1) ## Where is R2

### load MuMIn package to calculate R2 for mixed models
library(MuMIn)
r.squaredGLMM(lmm1)

### load performance package for different R2 calculation
### Note: For more complex models (eg. glmm's with multiple random effects, MuMIn and performance may differ)
library(performance)
r2(lmm1)

#############################################################################################################
## load in the ChickWeight dataset. It contains weight (g) of small chickens grown on four different diets.
## Chickens were weighed every few days for 21 days.
data("ChickWeight")
ChickWeight$Diet <- as.factor(ChickWeight$Diet)
?ChickWeight

## plot out data
ggplot(ChickWeight, aes(x=Time,y=weight))+geom_point()+facet_wrap(~Diet)+geom_smooth(method="lm")+theme_bw(base_size=16)

head(ChickWeight)

## Raneffs and blups
library(broom.mixed)
library(glmmTMB)
cwmod <- glmmTMB(weight ~ Time * Diet + (1|Chick), data=ChickWeight)
glance(cwmod)

cwblups <- tidy(cwmod, effect="ran_vals")
var(cwblups$estimate)


### regular linear model ignoring chick
cw0 <- lm(weight ~ Time * Diet , data=ChickWeight)
Anova(cw0)

### examine means at time 20
emmeans(cw0, pairwise~Diet, at=list(Time=20)) # All contrasts are significant

### construct a new model with chick as fixed effect
cw1a <- glmmTMB(weight ~ Time * Diet + Chick , data=ChickWeight)
Anova(cw1a)
summary(cw1a) ## look for R2

### examine means at time 20
emmeans(cw1a, pairwise~Diet, at=list(Time=20)) # Contrasts similar to above


### chick as a random (block) effect
cw1 <- lmer(weight ~ Time * Diet + (1|Chick), data=ChickWeight)
summary(cw1) ## look for variance component. Where is R2 ???

### Print off only variance component
cvar <- VarCorr(cw1)
print(cvar, comp=c("Variance","Std.Dev."))

### print off anova tables
anova(cw1) ## anova() from lmerTest package is best when using lmer()
Anova(cw1) ## Anova() from car package. Diet p-value is low, but note it uses Wald Chi-sq test. lmerTest::anova() is more appropriate in this case.

### examine model residuals
plot(resid(cw1)~fitted(cw1)) ## residuals not great, but we'll return to this later. For now we will proceed with caution.

### examine emmeans and contrasts
emmeans(cw1, pairwise~Diet, at=list(Time=20)) # 4 of 6 differ at time 20

###load MuMIn package to calculate R2 for the mixed-model################
r.squaredGLMM(cw1)

## Question: How much of the variance in weight is explained by Diet and Time? How much by Chick?
## Harder question: calculate the R2c by hand based (based on lecture notes) on the R2m and variance components (just to check)


#### more with variance components #########################################################

## calculate a rough approximation of the variance component.
## First we need to extract the random effect estimates. The 
randoms<-ranef(cw1, condVar = TRUE) ## extract the random effect estimates
rand.blups<-randoms$Chick 
head(rand.blups,15) ## print off the random effect estimates. The row labels are the Chick number and the (Intercept) values are the random effect estimates for each chick. Basically, each chick gets its own adjustment to the base intercept in the model.
## in this case the base intercept is 31.5143, so if you make the adjustments most of the indivdual chick intercepts are positive.


### Extract some values for plotting
qq <- attr(ranef(cw1, condVar = TRUE)[[1]], "postVar") ## convert list to vector
sd(qq)  ## approximate Variance Component. Note this basic calculatation usually biases the VC estimates downwards (they are too low) 
var(qq)  ## approximate Variance Component. 
df<-data.frame(Intercepts=randoms$Chick[,1],
               sd.interc=2*sqrt(qq[,,1:length(qq)]),
               Chick=rownames(rand.blups))
#df$Intercepts1 <- df$Intercepts+121.8183

cw1am <- emmeans(cw1a, ~Chick, at=list(Time=0)) %>% as.data.frame() %>% mutate(Chick=as.character(Chick)) # extract means for plotting later

df$Chick<-factor(df$Chick,levels=df$Chick[order(cw1am$Chick)])

df1 <- full_join(df,cw1am) 

## make plot of random effect BLUPs (best linear unbiased predictor)
p1 <- ggplot(df1,aes(x=Chick, y=Intercepts, color=Diet)) + geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin=Intercepts-sd.interc, ymax=Intercepts+sd.interc), width=0,color="black") + 
  geom_point(size=2) + xlab("Chick ID") + ylab("Intercepts") + 
  geom_hline(yintercept=-23.361, color="red") + geom_hline(yintercept=23.361, color="red") +
  ggtitle("Random effect BLUPs") + coord_flip() + theme_bw() 

## make plot of fixed-estimates intercetps for each chick from linear model (sometimes called BLUEs, best linear unbiased estimate)
p2 <- ggplot(df1,aes(x=Chick,y=emmean-31.5143, color=Diet)) + geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin=(lower.CL-31.5143), ymax=upper.CL-31.5143), width=0,color="black") + 
  geom_point(size=2) + xlab("Chick ID") + ylab("Intercepts") + ggtitle("Fixed effects EMMEANS") + coord_flip() + theme_bw()


## plot histogram of the random intercepts. Thin red bars represent 1 the SD of the variance component, which is similar (although not exactly the same) as 1 SD of the of the random intercepts
ggplot(df1, aes(x=Intercepts)) + geom_histogram(color="grey", bins=7) + 
  geom_vline(xintercept = 0, color="red", lwd=3) + geom_vline(xintercept = 23.361, color="red", lwd=2)+
  geom_vline(xintercept = -23.361, color="red", lwd=2) 

