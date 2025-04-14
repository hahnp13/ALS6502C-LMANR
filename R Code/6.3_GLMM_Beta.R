library(emmeans)
library(tidyverse)
library(car)
library(MuMIn)
library(DHARMa)
library(glmmTMB)
library(viridis)
library(performance)
library(ggeffects)
library(easystats) ## new package, may need to install (it has features that used to be in performance package)

# load data and filter out species not used in analysis ####
s1 <-read_csv("Palmer_percentseedloss.csv") %>% 
  filter(Species!='ANMI',Species!='ARLU',Species!='CAFI',Species!='ERPU',Species!='FECA',Species!='FEID',Species!='LIRU')
head(s1)
summary(s1)

## plot data
ggplot(s1, aes(x=Seed_loss_percent)) + geom_histogram() + theme_bw(base_size = 24)
ggplot(s1, aes(x=Seed_loss_percent)) + geom_histogram() + facet_wrap(~Species)

## provide a small transformation to the data to get rid of zeros and 1's for the beta distribution (or logit-transformation)
s1$SeedDmg <- (s1$Seed_loss_percent*(length(s1$Seed_loss_percent)-1)+.5)/length(s1$Seed_loss_percent)

# construct models ####

## ordbeta ####
mod1 <- glmmTMB(Seed_loss_percent ~ SeedSize_mg_log10 + (1|Species) + (1|Site), 
                family=ordbeta(link="logit"), data=s1)

## beta_family ####
mod2 <- glmmTMB(SeedDmg ~ SeedSize_mg_log10 + (1|Species) + (1|Site), 
                family=beta_family(link="logit"), data=s1)

## logit-transformation ### simplest mathematically, so sometimes best to use logit-transform
mod3 <- glmmTMB(logit(SeedDmg) ~ SeedSize_mg_log10 + (1|Species) + (1|Site), 
                 family=gaussian, data=s1)

## plot residuals. Note: simulated residuals never look good for beta distribution
plot(simulateResiduals(mod1))
plot(simulateResiduals(mod2))
plot(simulateResiduals(mod3))

check_model(mod1)
check_model(mod2)
check_model(mod3)

## Note: can't use AIC to compare these models because the response variable is different and families are different

## Look at coefficients to see how they change between families
## raw mean
mean(s1$Seed_loss_percent, na.rm=T)
## emmeans
emmeans(mod1, ~1, type="response") # mean for ordbeta; a little lower than raw mean (which is expected with skew)
emmeans(mod2, ~1, type="response") # almost same as the raw mean
emmeans(mod3, ~1, type="response") # very low; use caution!

## emtrends
emtrends(mod1, var="SeedSize_mg_log10", ~1) # slope for ordbeta model
emtrends(mod2, var="SeedSize_mg_log10", ~1) # slope beta_family model
emtrends(mod3, var="SeedSize_mg_log10", ~1) # slope for logit-transformation

# check for zero inflation ####

## ordbeta with zero inflation and ZI modeled by species (see hist on line 20) ####
mod1a <- glmmTMB(Seed_loss_percent ~ SeedSize_mg_log10 + (1|Species) + (1|Site), zi=~1,
                 family=ordbeta(link="logit"), data=s1)
mod1b <- glmmTMB(Seed_loss_percent ~ SeedSize_mg_log10 + (1|Site) , zi=~(1|Species),
                 family=ordbeta(link="logit"), data=s1)
mod1c <- glmmTMB(Seed_loss_percent ~ SeedSize_mg_log10 + (1|Site) , zi=~Species,
                 family=ordbeta(link="logit"), data=s1)
## Note: we can use AIC to compare the three ordbeta models with different zi terms

AIC(mod1,mod1a, mod1b, mod1c)

plot(simulateResiduals(mod1b))
check_model(mod1b)

plot(simulateResiduals(mod1c))
check_model(mod1c)

summary(mod1c)

## ZI ~Species model looks pretty good

## print summary
summary(mod1c)


## print Anova table
Anova(mod1c)

## look at emmeans/emmtrends
## raw mean
mean(s1$Seed_loss_percent, na.rm=T)
## emmeans
emmeans(mod1c, ~1, type="response", component="cond") # mean for ordbeta without the ZI component of the model
emmeans(mod1c, ~Species, type="response", component="zi") # mean for ZI component of the model
emmeans(mod1c, ~Species, type="response", component="response") # mean overall taking into account both the conditional and ZI components

## emtrends
emtrends(mod1c, var="SeedSize_mg_log10", ~1) # slope for ordbeta model

## r2
r2(mod1c) # problems R2c


# Make simple plot ####
### geom_smooth line is fit using all 1665 observations (a lot!)
beta1 <- ggplot() + 
  geom_jitter(data=s1, aes(x=SeedSize_mg_log10, y=Seed_loss_percent), size=2, color="grey", width=.01,height=0,alpha=.5) + 
  geom_smooth(data=s1, aes(x=SeedSize_mg_log10, y=SeedDmg), method="glm",method.args=list(family="ordbeta"), 
              formula = y ~ x, se=T, lwd=1.5, color="black")+
  labs(x='log10(Seed size) (mg)', y='Seed loss') +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 20) + 
  ggtitle("Line fit through all 1665 data points")
ggsave("BetaPlot1.tiff", beta1, width=8, height=6, units="in", dpi=600, compression = "lzw")

# Make excellent plot ####
## summarize data for plotting
s1s <- s1 %>% group_by(Species) %>% summarize(Seed_loss_percent=mean(Seed_loss_percent, na.rm=T),
                                              SeedSize_mg_log10=mean(SeedSize_mg_log10,na.rm=T),
                                              SeedDmg=mean(SeedDmg,na.rm=T))

### geom_smooth is fit through the 13 species means
### better for visualizing the model
beta2 <- ggplot() + 
  geom_jitter(data=s1, aes(x=SeedSize_mg_log10, y=Seed_loss_percent), size=2, color="grey", width=.01,height=0,alpha=.5) + 
  geom_point(data=s1s, aes(x=SeedSize_mg_log10, y=Seed_loss_percent), size=4) + 
  geom_smooth(data=s1s, aes(x=SeedSize_mg_log10, y=SeedDmg), 
              method="glm",method.args=list(family="ordbeta"), 
              formula = y ~ x, se=T, lwd=1.5, color="black")+
  labs(x='log10(Seed size) (mg)', y='Seed loss') +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 20)+ 
  ggtitle("Line fit through all 13 points for species means")

ggsave("BetaPlot2.tiff", beta2, width=8, height=6, units="in", dpi=600, compression = "lzw")

