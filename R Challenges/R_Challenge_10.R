### R Challenge 10 ###

### CHOOSE ONE OF THE PROBLEM SETS BELOW 1. OR 2.

#################################################################################################
#### 1. problem set w/ real data (Rivkin et al. 2018 Am J Bot) #####
#### herbivory data collected on 43 populations of an aquatic plant across a latitudinal gradient in Canada
#### At each population, many plants (~5-15) were examined for herbivory damage
#### Some additional covariates were recorded, such as Competition around the plant (1-3 from less to more) and plant height (cm)

### Q: Does herbivory increase towards the equator?
####    How do residuals look? Any way to improve them?

d1 <-read_csv("ajb21098-sup-0002-appendixs2.csv")
head(d1)
hist(d1$LeafDamage)

## removes 0's and 1's (see Smithson & Verkuilen 2006 or Douma & Weedon 2018)
d1$LeafDamage1 <- (d1$LeafDamage*(length(d1$LeafDamage)-1)+.5)/length(d1$LeafDamage)

ggplot(d1 , aes(x=Latitude, y=LeafDamage1)) + geom_point() +
  geom_smooth(method='glm', method.args=list(family="beta_family"), formula = y~x)


lm1 <- glmmTMB(LeafDamage ~ Latitude +  (1|Population), family="beta_family", data=d1)
summary(lm1)

plot(simulateResiduals(lm1))

#################################################################################################
#### 2. problem set w/ real data (Rivkin et al. 2018 Am J Bot) #####
####    Rats dataset. (use only females). Half treated with drug and untreated and then checked for tumor.
####    Does the drug reduce probability of developing tumor? AFter 50 days? After 100 days?

library(survival)
rats <- rats %>% filter(sex=="f")
?rats
head(rats)

ggplot(rats , aes(x=time, y=status)) + geom_point() +
  geom_smooth(method='glm', method.args=list(family="binomial"), formula = y~x) + 
  facet_wrap(~rx)

rat1 <- glmmTMB(status ~ as.factor(rx) * time + (1|litter), family="binomial", data=rats)

plot(simulateResiduals(rat1))

Anova(rat1)
summary(rat1)

emmeans(rat1, pairwise ~ rx, type='response', at=list(time=50))
emmeans(rat1, pairwise ~ rx, type='response', at=list(time=100))
emmeans(rat1, pairwise ~ rx, time=50)
