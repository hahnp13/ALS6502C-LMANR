library(bbmle)
library(viridis)
library(tidyverse)
library(MuMIn) ## new package
library(ggeffects)
library(car)
library(GGally)
library(glmmTMB)
library(easystats)

# load beetle size dataset and examine #############################################
df<-readRDS("R Code/size_data.rds")
glimpse(df)
summary(df) # provides some summary statistics

## explore data prior to beginning ####
ggpairs(df %>% select(SR,MAT,ATR,NPP_men))
hist(df$geo_avg)
hist(log(df$geo_avg))

# correlated model
corrmod <- glmmTMB(log(geo_avg) ~ ATR + MAT , data = df)
check_collinearity(corrmod) # from performance package

### need to scale variables first so they are comparable ####
df1 <- df %>% mutate(SR=scale(SR)[,1], ATR=scale(ATR)[,1], MAT=scale(MAT)[,1], NPP_men=scale(NPP_men)[,1])

ggpairs(df1 %>% select(SR,MAT,ATR,NPP_men))

##  QUESTION: How does "scaling" change the relationship? How does it change the correlations? How does it change the units/values?

## create set of candidate models ####

#1 null model
nullm<-glmmTMB(log(geo_avg) ~ 1, data = df1)

#2 sampling model
sample<-glmmTMB(log(geo_avg) ~ SR, data = df1)

#3 seasonality model
seasonality<-glmmTMB(log(geo_avg) ~ ATR, data = df1)

#4 temp model
TEMP<-glmmTMB(log(geo_avg) ~ MAT, data = df1)

#5 NPP model
NPP<-glmmTMB(log(geo_avg) ~ NPP_men, data = df1)

#6 season and SR
seasonSR <- glmmTMB(log(geo_avg) ~ ATR + SR , data = df1)

#7 npp and richness 
nppsr <- glmmTMB(log(geo_avg) ~ NPP_men + SR , data = df1)

#8 npp and atr 
nppatr <- glmmTMB(log(geo_avg) ~ NPP_men + ATR , data = df1)

#9 global 
global <- glmmTMB(log(geo_avg) ~ ATR + NPP_men + SR , data = df1)

### QUESTION: why other models to make? Up to 9 or 10 could be reasonable. If you make more models, add them into the AICctabs below

## print off AIC for each model ####
AIC(nullm, sample, seasonality, TEMP, NPP, seasonSR, global,nppsr,nppatr) %>% as.data.frame() %>% arrange(AIC)

## compare model AIC using easystats ####
?compare_performance
compare_performance(nullm, sample, seasonality, TEMP, NPP, seasonSR, global,nppsr,nppatr, rank=T)

## compare with bblme package ####
AICtab(nullm, sample, seasonality, TEMP, NPP, seasonSR, global,nppsr,nppatr,
       weights = T, delta = T, base = T, sort = T)


## look at summary of global ####
summary(global)
model_parameters(global)
model_performance(global) ## look at model performance metrics for global modelob)
check_model(global)

# Model averaging #### (not covered in class)
## average top models within 2 AICc using MuMIn package ####
topmodsall <- model.avg(modtable, subset = delta < 2)
summary(topmodsall)

topmods <- model.avg(seasonSR,seasonality,global)
summary(topmods)

## same example in dredge ####
## Note: dredge will automatically run all possible subset models from a global model
## Note2: This can very very convienent because you don't have to type out all the models
## Note3: Use caution though because there can be submodels that don't really make sense
## Note4: You can specify variabiles to have in all models or only a certain number of terms. See ?dredge
options(na.action = "na.fail") # need to run this, for some reason

dredge.mod <- dredge(global)
dredge.mod # compare to modtable from above

topmodsall.d <- model.avg(dredge.mod, subset = delta < 2)
summary(topmodsall.d) # identical to summary(topmodsall) from above

## use ggpredict from ggeffects package to make plots ####
## with multiple predictor variables, its important to hold other constant when examining the relationship between a variable and y
## for example, for a model y~x1+x2, when examining y~x1 you must hold x2 constant (ie. control for x2)
## Note: CI not calculated on averaged model, not sure why

topmodspred <- ggpredict(topmodsall, terms=c("SR","NPP_men","ATR"))

## plot all terms 
plot(topmodspred)

## plot only important terms
plot(ggpredict(topmodsall, terms=c("ATR","SR")), add.data=T)

## maybe we just want to show only ATR, controlling for the other vars in the model
plot(ggpredict(topmodsall, terms=c("ATR")), add.data=T) + theme_bw(base_size = 16)

## plot raw data
atr_sr_plot <- ggplot(data = df) +
  geom_point(aes(x = ATR, y = geo_avg, fill = log(SR)),pch = 21,size = 5,color = 'grey', alpha = 0.8) +
  scale_fill_viridis_c() +
  #scale_y_log10() +
  labs(x = "Annual Temp. Range",
       y = "Size", fill = "Log(SR)") +
  geom_smooth(aes(x = ATR, y = geo_avg), method = "lm",color = "black") +
  ylim(c(0, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        legend.position = "top") 
ggsave("ModSelectionPlot.png", atr_sr_plot, width = 6, height = 4, units = "in", dpi = 300)
