library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(DHARMa)
library(viridis)
library(glmmTMB)
library(easystats)

# LOAD TITANIC SURVIVAL DATASET ####
data("TitanicSurvival")
t1 <- TitanicSurvival %>% filter(age>17) # filter out children
head(t1)

## plot data ####
ggplot(t1, aes(x=passengerClass, y=survived, color=sex)) + geom_jitter(height=.2, width=0.2)

## construct a generalized linear model to estimate survival as a function of sex and passengerClass. Include Age as co-variate.
tglm1 <- glmmTMB(survived ~ sex * passengerClass + age, data=t1, family = binomial(link = "logit"))

## print off anova table
Anova(tglm1)

## print off summary
summary(tglm1)

## Check residuals
hist(residuals(tglm1)) ## residuals should be normally distributed, even for glm
plot(residuals(tglm1)~fitted(tglm1)) +  ## residuals should be evenly dispersed around 0 across the range of x's
  abline(h=0)                               # funnel shapes or curvature is bad

qqPlot(residuals(tglm1))  ## residuals should line up pretty closely to the blue line
boxplot(residuals(tglm1) ~ t1$passengerClass)  ## variances should be homogeneous for each group

## simulate residuals
hist(simulateResiduals(tglm1))
plot(simulateResiduals(tglm1))

check_overdispersion(tglm1)

check_model(tglm1) ## looks weird, not sure why (I think it struggles because response is yes/no). Model should be OK.

## print out emmeans
emmeans(tglm1, pairwise ~ sex:passengerClass)

## print out back-transformed means
emmeans(tglm1, pairwise ~ sex:passengerClass, type="response")  ## type= does contrasts before back-transforming (most appropriate!)

#### create new variable that is 0 or 1 for plotting
t1$surv <- if_else(t1$survived=='yes',1,0)

## make plot with regression lines
ggplot(t1, aes(x=age, y=surv, color=sex)) + geom_jitter(height=.1, width=0) + 
  geom_smooth(method="glm",method.args=list(family="binomial"), formula = y ~ x, se=F, linewidth=1.5) +
  facet_wrap(~passengerClass) + theme_bw(base_size = 20)

## make nice plot #####
tm <- emmeans(tglm1, ~ sex:passengerClass, type="response") %>% as.data.frame()

plot1 <- ggplot() +
  geom_jitter(data=t1 %>% filter(sex=='female'),
              aes(x=passengerClass, y=surv+.01, color=sex),
              height=0, width=.25, size=1, alpha=.1) +
  geom_jitter(data=t1 %>% filter(sex=='male'), 
              aes(x=passengerClass, y=surv-.01, color=sex), 
              height=0, width=.25, size=1, alpha=.1) +
  geom_errorbar(data=tm,
                aes(x=passengerClass,
                    y=prob, ymin=(prob-SE), 
                    ymax=(prob+SE), color=sex), 
                width=.2, lwd=1.25,
                position = position_dodge(width=0.5)) +
  geom_point(data=tm , 
             aes(x=passengerClass, y=prob, color=sex), 
             size=5, position=position_dodge(width=0.5)) + 
  scale_y_continuous('survival', labels = scales::percent) +
  scale_color_viridis(discrete = T, end=.8) +
  theme(panel.background = element_blank(),                           
        panel.border = element_rect(color="black",
                                    fill=NA, linewidth = 2)) +
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x =  element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(text = element_text(size=20)) 
plot1

plot2 <- ggplot() +
  geom_jitter(data=t1 ,
              aes(x=passengerClass, y=surv, 
                  color=passengerClass), 
              height=.01, width=.35, 
              size=1, alpha=.2) +
  geom_errorbar(data=tm ,
                aes(x=passengerClass,
                    y=prob, ymin=(prob-SE),
                    ymax=(prob+SE), 
                    color=passengerClass), 
                width=.2, lwd=1.25) + ## make bars thinner
  geom_point(data=tm , 
             aes(x=passengerClass, 
                 y=prob, color=passengerClass), size=5) +
  facet_wrap(~sex) +
  scale_y_continuous('survival', labels = scales::percent) +
  scale_color_viridis(discrete = T, option = 'C', direction=-1, end=.85) +
  theme(panel.background = element_blank(),                           
        panel.border = element_rect(color="black", fill=NA, linewidth=2)) +
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) + 
  theme(text = element_text(size=20)) 
plot2

## plot with Age
ggplot() + 
  geom_jitter(data=t1 %>% filter(sex=='female'),
              aes(x=age, y=surv+.01, color=sex),
              height=0, width=.25, size=1, alpha=.2) +
  geom_jitter(data=t1 %>% filter(sex=='male'), 
              aes(x=age, y=surv-.01, color=sex), 
              height=0, width=.25, size=1, alpha=.2) +
  geom_smooth(data=t1, aes(x=age, y=surv, color=sex),
              method="glm",method.args=list(family="binomial"), formula = y ~ x, se=T, linewidth=1.5) +
  scale_y_continuous('survival', labels = scales::percent) +
  scale_color_viridis(discrete = T, end=.8) +
  facet_wrap(~passengerClass) + 
  theme_bw(base_size = 20)



