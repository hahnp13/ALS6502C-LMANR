library(emmeans)
library(tidyverse)
library(car)
library(MuMIn)
library(DHARMa)
library(glmmTMB)
library(viridis)
library(performance)
?library(ggeffects)

# brms: new package. Install if needed. 
#   May need to run the two lines below if your RTools is not compatible
#    (you will get an error message saying your Stan code cannot compile)
library(brms)
# remove.packages(c("rstan", "StanHeaders"))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# load data and filter out species not used in analysis ####
s1 <-read_csv("Palmer_percentseedloss.csv") %>% 
  filter(Species!='ANMI',Species!='ARLU',Species!='CAFI',Species!='ERPU',Species!='FECA',Species!='FEID',Species!='LIRU')
head(s1)
summary(s1)

## plot data ####
ggplot(s1, aes(x=Seed_loss_percent)) + geom_histogram() + theme_bw(base_size = 24)
ggplot(s1, aes(x=Seed_loss_percent)) + geom_histogram() + facet_wrap(~Species)

## provide a small transformation to the data to get rid of zeros and 1's for the beta distribution (or logit-transformation) ####
s1$SeedDmg <- (s1$Seed_loss_percent*(length(s1$Seed_loss_percent)-1)+.5)/length(s1$Seed_loss_percent)

# Construct models in glmmTMB ####
mod1 <- glmmTMB(SeedDmg ~ SeedSize_mg_log10 + (1|Species) + (1|Site), 
                family=beta_family(link="logit"), data=s1)

## alt model just for demoing the logit-transformation ####
mod1a <- glmmTMB(logit(SeedDmg) ~ SeedSize_mg_log10 + (1|Species) + (1|Site), 
                 family=gaussian, data=s1)

## plot residuals. Note: simulated residuals never look good for beta distribution
##   There is probably some zero-inflation in the data, but for now we will proceed.
plot(simulateResiduals(mod1))
plot(simulateResiduals(mod1a))

testZeroInflation(simulateResiduals(mod1)) # no actual zero's in dataset, so this won't really do anything

## print summary
summary(mod1)

## print Anova table
Anova(mod1)

## r2
r2(mod1)
 
# bayesian version of model ####
brms1 <- brm(SeedDmg ~ SeedSize_mg_log10 + (1|Species) + (1|Site), 
                family=Beta(link = "logit", link_phi = "log"), data=s1, 
             prior=NULL, 
             iter=1000) ## iterations should be at least 2000 but takes longer

## look at default priors ####
get_prior(SeedDmg ~ SeedSize_mg_log10 + (1|Species) + (1|Site),data=s1)


## examine postior density and trace plots ####
plot(brms1)

## posterior predictive check plots for model ####
##  similar to checking residuals. Light blue lines should overlay dark line.
##  its possible to make DHARMa work with brms but takes some doing.
pp_check(brms1, ndraws = 100)

## summary of model ####
summary(brms1) # compare to glmmTMB model, very similar

## make plot of parameter estimates in ggplot 
##  (very helpful! especially for more complex models)
conditional_effects(brms1)

## hypothesis testing, is slope greater than 0?
hypothesis(brms1, "SeedSize_mg_log10 > 0")


## r2
r2(brms1) ## lower than glmmTMB but similar


# zero inflated beta ####
### need to keep 0's but remove 1, so we subtract a tiny value only from the 1's
### there is a zero_one_inflated_beta family, which could be an alternative option

s1$SeedDmg0 <- ifelse(s1$Seed_loss_percent==1,s1$Seed_loss_percent-.001,s1$Seed_loss_percent)


## construct model in brms using zero inflated beta family
brms2 <- brm(SeedDmg0 ~ SeedSize_mg_log10 + (1|Species) + (1|Site), 
             family=zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"), 
             data=s1,
             iter=1000)

## examine postior density and trace plots
plot(brms2)

## posterior predictive check plots for model
pp_check(brms2, ndraws = 100)


## summary of model
summary(brms2)
# the zi parameter will be back-transformed form the logit-link 
#  so it represents the probability of an extra zero for each observation

## make plot of parameter estimates in ggplot (very helpful!)
conditional_effects(brms2)

## hypothesis testing, is slope greater than 0?
hypothesis(brms2, "SeedSize_mg_log10 > 0")

## r2
r2(brms2)

# zero inflated beta with varying zi parameter ####

## construct model in brms using zero inflated beta family varying by species
brms3 <- brm(bf(SeedDmg0 ~ SeedSize_mg_log10 + (1|Species) + (1|Site), zi~Species), 
             family=zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit"), 
             data=s1,
             iter=1000)

## examine postior density and trace plots
plot(brms3)

## posterior predictive check plots for model
pp_check(brms3, ndraws = 100)

## summary of model
summary(brms3)

## make plot of parameter estimates in ggplot (very helpful!)
conditional_effects(brms3)


## r2
r2(brms2)



#### Make simple plot ####
### geom_smooth line is fit using all 1665 observations (a lot!)
beta1 <- ggplot() + 
  geom_jitter(data=s1, aes(x=SeedSize_mg_log10, y=Seed_loss_percent), size=2, color="grey", width=.01,height=0,alpha=.5) + 
  geom_smooth(data=s1, aes(x=SeedSize_mg_log10, y=SeedDmg), method="glm",method.args=list(family="beta_family"), 
              formula = y ~ x, se=T, lwd=1.5, color="black")+
  labs(x='log10(Seed size) (mg)', y='Seed loss') +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 20) + 
  ggtitle("Line fit through all 1665 data points")
ggsave("BetaPlot1.tiff", beta1, width=8, height=6, units="in", dpi=600, compression = "lzw")

#### Make excellent plot ####
## summarize data for plotting
s1s <- s1 %>% group_by(Species) %>% summarize(Seed_loss_percent=mean(Seed_loss_percent, na.rm=T),
                                              SeedSize_mg_log10=mean(SeedSize_mg_log10,na.rm=T),
                                              SeedDmg=mean(SeedDmg,na.rm=T))

### geom_smooth is fit through the 13 species means
### better for visualizing the model
beta2 <- ggplot() + 
  geom_jitter(data=s1, aes(x=SeedSize_mg_log10, y=Seed_loss_percent), size=2, color="grey", width=.01,height=0,alpha=.5) + 
  geom_point(data=s1s, aes(x=SeedSize_mg_log10, y=Seed_loss_percent), size=4) + 
  geom_smooth(data=s1s, aes(x=SeedSize_mg_log10, y=SeedDmg), method="glm",method.args=list(family="beta_family"), 
              formula = y ~ x, se=T, lwd=1.5, color="black")+
  labs(x='log10(Seed size) (mg)', y='Seed loss') +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 20)+ 
  ggtitle("Line fit through all 13 points for species means")

ggsave("BetaPlot2.tiff", beta2, width=8, height=6, units="in", dpi=600, compression = "lzw")

