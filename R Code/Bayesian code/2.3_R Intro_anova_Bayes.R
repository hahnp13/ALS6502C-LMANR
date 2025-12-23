## LOAD AND PROCESSES DATA
library(tidyverse)   ### load tidyverse
library(car)         ### load car package, which is helpful for analyzing linear models
library(emmeans)     ### load emmeans package, which is helpful for getting means from linear models
library(viridis)     ### load viridis package for adding colors to plots
library(brms)
library(cmdstanr)
library(broom.mixed)
library(easystats)
library(loo)

# EXAMPLE FOR CONDUCTING A ONE-WAY ANOVA IN R using brms #################################

## load data ####
data("InsectSprays") ### load InsectSprays dataset, available from base R  

# filter to just 4 treatments
d <- InsectSprays %>% filter(spray=='A'|spray=='B'|spray=='C'|spray=='F') %>% droplevels()

# STEP 0. explore and plot out data ####
ggplot(d, aes(x=spray,y=count)) + geom_boxplot(outlier.shape = NA) + geom_jitter(height=0,width=.1) 
## need to suppress outliers if you jitter plot points

hist(d$count) ## histogram of count data. Can we use to check assumption of normality?

# STEP 1. construct linear model to examine the effect of the different sprays on insect counts ####
## for a categorical variable (spray with four levels), we are interested in comparing group means

## check and set priors ####
get_prior(bf(count ~ spray), data=d)

priors_bm2 <- c(prior(normal(12,5), class="Intercept"),
                prior(normal(0,10), class="b"))

bm2 <- brm(count~spray , data=d,
           iter=2000, warmup=1000,
           prior = priors_bm2,
           save_pars = save_pars(all = TRUE),
           sample_prior = "yes",
           backend = "cmdstanr")  
## brm is a general function that conducts a linear model using the program STAN
# all the "calculations" are saved in an object we called 'bm2'


# STEP 2. check assumptions of model by examining residuals ####
pp_check(bm2) ## looks reasonable, could be better (we will return to this example later in class)


## next we will plot density (histograms) and trace (chains) plots of the MCMC draws
plot(bm2)  ## histograms show the posterior draws for each parameter estimated by the model
# remember, just like glmmTMB, the estimates for Spray B,C,F are adjustments to the intercept (which is Spray A)
# so, we will have to recalucate those group means in a Step 5



# STEP 3. examine coefficients and model components ####
summary(bm2)   ## summary() will provide the model coefficients (ie. the "guts" of the model)
# the coefficients allow you rebuild the means from the linear model equation y~u+Bi
# rebuilding the model from the coefficients is not super helpful with multiple categories/groups and the p-values aren't very meaningful

## you can use tidy(), glipse(), etc but again this step isn't all that useful. Check summary to make sure it looks good and move on


# STEP 4. Check "significance" with an omnibus test ####

# car::Anova doesn't work because we aren't testing significance in the traditional sense
## need to make a "null" model that only calculates a single intercept (ie. one grand mean)
## then we will compare this null model to the other model that contains 'spray'
bm2_null <- brm(count~1 , data=d,
           iter=2000, warmup=1000,
           prior = prior(normal(12,5), class="Intercept"), ## only parameter is intercept
           save_pars = save_pars(all = TRUE),
           sample_prior = "yes",
           backend = "cmdstanr")  


## bayes factor will calculate an evidence ratio
bayes_factor(bm2, bm2_null) ## Its WAY over 10 so massive evidence of a 'spray' effect


## or you can use loo (leave one out comparisons), which is similar to AIC model selection that we will talk briefly about in Module 3.3
## may need to install the 'loo' package
loo_compare(loo::loo(bm2_null), loo::loo(bm2))
## here, the null has a much worse loo because it is more than 2x the se_diff


# STEP 5. examine group means ####
emmeans(bm2, ~spray) ## emmeans::emmmeans will rebuild the model for you
# this code will print off the means and credible intervals for each treatment group

# STEP 6. pairwise comparisons of group means ####
emmeans(bm2, pairwise~spray)  ## adding 'pairwise' will conduct pairwise contrasts -- ie. compare each group mean to the others
## comparisons where the credible intervals don't overlap zero are considered important (or "significant")
## this is a quick way to compare the means, not necassarily the best, but passable.

## a more elaborate, but powerful way using emmeans
emm <- emmeans(bm2, ~spray) # extract means
emm_pw <- pairs(emm)  # extract pairwise comparisons (same as second table from above)

test(emm_pw, delta = 10) # need to specify how different the means should be, in this case 10 (this value is a little arbitrary)

## or we can compare each to "F" the control
contrast(emm, method = "trt.vs.ctrl", ref = "F")


## pairwise tests with hypothesis statement ####
## in this example, we are mainly interested in comparing each spray to the control 'sprayF'
## but first I'm going to refit the model to estimate only group means and not an intercept
bm2_noInt <- brm(count~ 0 + spray , data=d,
           iter=2000, warmup=1000,
           prior = prior(normal(10,10), class="b"), # adjust prior because we have no intercept and only 'b' coefficients
           save_pars = save_pars(all = TRUE),
           sample_prior = "yes",
           backend = "cmdstanr")  

pp_check(bm2_noInt) # just check
plot(bm2_noInt) # just check
summary(bm2_noInt) # just check

### now setup the hypotheses of interest ####
## use < because we predict that the sprays will work (ie. reduce insect counts)
bm2_hyp <- c("sprayA < sprayF", "sprayB < sprayF", "sprayC < sprayF") 

hypothesis(bm2_noInt, bm2_hyp)

# can use bayestestR package to do more sophisticated comparisons

# FINAL STEP: Plot data with means ####
## Plot data and add means plus SE from your emmeans. Can change colors, if you'd like.
bem50 <- emmeans(bm2, ~spray, level=.5) %>% as.data.frame()
bem80 <- emmeans(bm2, ~spray, level=.8) %>% as.data.frame()
bem95 <- emmeans(bm2, ~spray, level=.95) %>% as.data.frame() # probably can use plot the 95% CI

ggplot() + 
  geom_boxplot(data=d, aes(x=spray,y=count), outlier.shape = NA) + 
  geom_jitter(data=d, aes(x=spray,y=count), height=0,width=.1, size=2) + 
  geom_pointrange(data=bem50, aes(x=spray, y=emmean, ymin=lower.HPD, ymax=upper.HPD),
                  color="purple", size=1.25, linewidth=3) + ##geom_errorbar() call also be used to draw error bars
  geom_pointrange(data=bem80, aes(x=spray, y=emmean, ymin=lower.HPD, ymax=upper.HPD),
                  color="purple", size=1.25, linewidth=2) + ##geom_errorbar() call also be used to draw error bars
  geom_pointrange(data=bem95, aes(x=spray, y=emmean, ymin=lower.HPD, ymax=upper.HPD),
                  color="purple", size=1.25, linewidth=1) + ##geom_errorbar() call also be used to draw error bars
  theme_bw(base_size = 14)





