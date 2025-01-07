#### R CHALLENGE 5
library(tidyverse)
library(agridat) 
library(GGally)
library(skimr)
library(bbmle)
library(MuMIn)
library(performance)

## Examine the ortiz.tomato.yield dataset. The help gives decent metadata (but it's not great).
## The study grew several tomato genotypes at 18 sites around the world
## There are two datasets, the ortiz.tomato.cov contains site-level information.
## The ortiz.tomato.yield dataset contains data on yield for a bunch of plants grown at each site.
?ortiz.tomato

## Preparation: merge yield and covs dataset and select variables of interest
data("ortiz.tomato.yield")
data("ortiz.tomato.covs")

## today we will work with one genotype (OP15) rather than all 15 genos.
yield <- ortiz.tomato.yield %>% filter(gen=='OP15') %>% droplevels()  ## filter data. 
## Need to use 'droplevels()' otherwise it "remembers" the levels that are filtered out. Sometimes it is problematic.

## We will select just a few predictor variables based on our knowledge of the system and exploratory analysis
## The response variable is 'weight' (we won't look at yield here)
dat1 <- full_join(ortiz.tomato.covs,yield,by="env") %>% 
  as_tibble() %>% na.omit() %>%
  mutate(Driv=as.factor(Driv), Trim=as.factor(Trim), Irr=as.factor(Irr)) %>% 
  select(env,Driv,Irr,Day,MxT,Prec,weight)

## examine data structure. Note we are selecting just a few variables to focus on.
head(dat1)
str(dat1)
glimpse(dat1)
skim(dat1)


########################################################################################################################
#### STEP 1: We need to first standardize the predictor variables because they are on different scales.
####         We do this by centering each variable around zero and then scaling to its standard deviation.
####         Notice the new range of all the predictor variables.
?scale

dat1 <- dat1 %>% mutate(Day=scale(Day)[,1],MxT=scale(MxT)[,1],Prec=scale(Prec)[,1])
head(dat1)

#########################################################################################################################
#### Q1. We are interested in determining which environmental factors are most strongly correlated with tomato 'weight' 
####     (so, weight is response. Remember we selected just a few variables to consider as predictors)
####     Examine the ggpairs plot below. Which variables seem most important?
ggpairs(dat1 %>% select(-env))


#########################################################################################################################
#### Q2. Construct up to 10 candidate models with different combinations of variables in dat1
####     Each model should represent a hypothesis and could include 1-3 predictor variables
####     At a minimum, have models for each variable along, plus various combinations. Also include a null model.
####     Or, you can create a global model and use dredge()
options(na.action = "na.fail") # need to run this line for dredge, for some reason


#######################################################################################################################
#### Q3. Check potential co-linearity of predictor variables, at least for full model
####     Use performance::check_colinearity() to check for values over 2 indicate slight co-linearity. Values over 10 are problematic.



########################################################################################################################
#### Q4. Use AICc model selection to select the "best" model. Use AICctab rather than AICtab
####     What model is the "best"? Which models are within 4 AIC units. 6 units?

AICctab(ADDMODELSHERE, base=T, weights=T, delta=T)

### alternatively, you can use model.sel() from the MuMIn package, which provides more info


#######################################################################################################################
#### Q5. Which variable has the biggest effect on tomato weight? (hint: look at the estimates from summary() on your best model)
####     Remember that the variables are scales so the estimates can be compared directly.



########################################################################################################################
#### BONUS: use MuMIn package to average the top models. If several models are within 4 AICc units, you may need to average them.
#### See additional information in Harrison et al. (2018) reading for class on 2/17/22.
library(MuMIn) ## install package if needed
?MuMIn

modtab1 <- model.sel(ADDMODELSHERE) ## add models in here
modtab1

plot(modtab1)

topmods <- model.avg(modtab1, subset = delta < 4)
summary(topmods)
