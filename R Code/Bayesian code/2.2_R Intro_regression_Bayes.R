# EXAMPLE OF BAYESIAN LINEAR REGRESSION IN R #############################################
library(tidyverse)
library(car)
library(brms)
library(cmdstanr)
library(broom.mixed)
library(easystats)

set.seed(21)
temp <- round(runif(20,12,30), 2)            
mass <- round(rnorm(20,5*temp,25), 2)
r1 <- as.data.frame(cbind(temp,mass)) ### run lines 7-10 to generate some fake data

head(r1) ## temp is rearing temperature (C), mass is the mass of adults (mg)

# STEP 0. Examine and plot data ####
summary(r1)

hist(r1$mass)
ggplot(r1, aes(x=temp, y=mass))+
  geom_point(size=3)+
  theme_bw(base_size=20)

# STEP 1. build model ####
## construct a linear model to estimate the average adult mass per degree C of temperature increase
## for a continuous variable (temp in degree C), we are interested in estimating the slope between age and circumference
## Using brms, we simply use the 'brm' function exactly like we would use glmmTMB() or lm() etc.

## There are lots of options we can add, which we will need to do for more complicated models.
## But, we do need to get some (wide) priors.
## This simple model would work just fine with the default priors, but its good practice to set priors yourself 
##  and with this simple model, the priors are easy to set.
## Setting priors also helps you understand the parameters, so extra learning bonus.

## first check default priors
get_prior(bf(mass~temp), data=r1)

## set priors for the two parameters the model will estimate, the intercept and slope (it also estimates the standard deviation or sigma, but the default ok)
priors <- c(prior(normal(0,50), class="Intercept"),
            prior(normal(0,10), class="b"))


### now setup the model in brm ####
## if you have any issues, you can hashtag-out everything except the first line and it will run with the defaults
bm1 <- brm(mass~temp, data=r1,
           iter=2000, warmup=1000, chains = 4,
           #prior = priors,
           #save_pars = save_pars(all = TRUE),
           #sample_prior = "yes",
           #backend = "cmdstanr"
           )


# STEP 2. check assumptions of model ####

## pp_check provides a plot to check how well the model (ie. posterior draws) fit the data
##   What you will see is the distribution of the posterior draws (10 light blue lines) and the distrbution of the data (dark line)
##   basically the light blue lines (model results) should match closely to the data
# ?bayesplot::pp_check

pp_check(bm1) ## looks reasonable, lots of variation in posterior draws (light blue lines) but are similar to the data (dark line) 
              ## probably we should use some weakly informed priors, but we will return to that later


## next we will plot density (histograms) and trace (chains) plots of the MCMC draws
plot(bm1)  ## histograms show the posterior draws for each parameter estimated by the model
           ## these full distributions are what we will see summarized in the steps below
           ## Note the values for the intercept are centered around ~10 and ~4.5 for the slope
           
           ## The plots on the right show the each draw (ie. iteration) for the 4 chains that were run
           ## since we ran 1000 warmups and 2000 total iterations, we will have 1000 post-warmup draws for each chain
           ## what we want to see is that all the chains should overlay on top of each other. If one or more don't overlap, we have divergent chains indicating a problem with the model  
           ## note the intercept chains are centered on ~10 and the temp slope is ~4.5 (same as the histos)


## now we will use a more comprehensive model check from the easystats package
check_model(bm1)

## STEP 3: Look at model coefficients ####
summary(bm1)        ## summary() will provide the model coefficients (ie. the "guts" of the model)
# the coefficients allow you rebuild the means from the linear model equation y~B0+B1*X
# this table is similar to the glmmTMB model, the parameters are almost identical
# we get additional information here, including the lower and upper credible intervals
# Rhat is a check of how good the model fits, it should be 1.00 or within ~0.2. High values indicate problems
# bulk and tail ess are effective samples sizes. We had 4000 post-warmup draws, so that is the highest value they can be. These fine, good to have >1000 (idealy much more)
# we also get the variance estimate (sigma), which is not directly reported for the glmmTMB model


### other ways to get model coefficients ####
model_parameters(bm1)   ## look at model coefficients

estimate_prediction(bm1) ## this will print off the fitted vales and residuals


# STEP 4. Check significance ####
hypothesis(bm1, "temp > 0") ## model_parameters above is similar, can also use estimate_slopes()
estimate_slopes(bm1)

# check R2 for fit
r2(bm1) ## calculate R2


## quick plot ####
plot(conditional_effects(bm1), points=T)

plot(estimate_relation(bm1))
## later we will learn how to make customized plots

