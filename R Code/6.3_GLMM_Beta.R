library(emmeans)
library(tidyverse)
library(car)
library(MuMIn)
library(DHARMa)
library(glmmTMB)
library(viridis)
library(performance)
library(ggeffects)

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
mod1a <- glmmTMB(SeedDmg ~ SeedSize_mg_log10 + (1|Species) + (1|Site), 
                family=beta_family(link="logit"), data=s1)

## logit-transformation ### simplest mathematically, so sometimes best to use logit-transform
mod1b <- glmmTMB(logit(SeedDmg) ~ SeedSize_mg_log10 + (1|Species) + (1|Site), 
                 family=gaussian, data=s1)

## plot residuals. Note: simulated residuals never look good for beta distribution
##   There is probably some zero-inflation in the data, but for now we will proceed.
plot(simulateResiduals(mod1))
plot(simulateResiduals(mod1a))
plot(simulateResiduals(mod1b))

## print summary
summary(mod1)
summary(mod1a)
summary(mod1b)

## print Anova table
Anova(mod1)

## emmeans
emmeans(mod1, ~1, type="response") # A little lower than beta_family
emmeans(mod1a, ~1, type="response") # a little higher than ordbeta
emmeans(mod1b, ~1, type="response") # much lower than beta distribution; use caution!

## emtrends
emtrends(mod1, var="SeedSize_mg_log10", ~1) # slope a little lower than beta_family
emtrends(mod1a, var="SeedSize_mg_log10", ~1) # slope a little higher than ordbeta
emtrends(mod1b, var="SeedSize_mg_log10", ~1) # slope way higher than beta distribution

## r2
r2(mod1) # problems R2c
r2(mod1a) # problems with beta distribution
r2(mod1b) # R2 seem reasonable

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

