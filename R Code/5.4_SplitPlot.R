library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)
library(agridat) ## install and load package for datasets
library(multcomp) ## install and load package for multiple comparisons
library(glmmTMB)
library(car)

## load rice experiment and process data
data("gomez.multilocsplitplot")
gomez.multilocsplitplot$nitro <- as.factor(gomez.multilocsplitplot$nitro)
gomez <- gomez.multilocsplitplot
head(gomez)


## plot data
ggplot(gomez, aes(x=gen, y=yield, fill=nitro))+geom_boxplot(outlier.shape = NA)+
  geom_point(position = position_jitterdodge(jitter.height=0,jitter.width=.1))+facet_wrap(~loc)

## average data by loc and nitro to account for pseudo-replication (n = 36 plots)
gomez_summarized <- gomez %>% group_by(loc,nitro,gen) %>% summarize(yield=mean(yield, na.rm=T))


## Regular two-way anova using summarized dataset -- no blocking 
mm0 <- lm(yield ~ gen*nitro, data=gomez_summarized)
anova(mm0)

## Two-way anova with block as a random effect
mm1 <- glmmTMB(yield ~ gen*nitro+(1|loc), data=gomez_summarized)
Anova(mm1)

## Two-way anova with block and nitro nested within block as random effects
mm2 <- lmer(yield ~ gen*nitro+(1|loc/nitro), data=gomez_summarized)
Anova(mm2)
anova(mm2)

## summary for split-plot model
summary(mm2)

## emmeans for just for nitro (let's not worry about genotype)
emmeans(mm2, pairwise~nitro)

## Two-way anova with block and nitro nested within block as random effects
## Fully embrace the nestedness and include plot as a random effect instead of averaging (n=108 data points, use them all!)
mm3 <- lmer(yield ~ gen*nitro+(1|loc/nitro/gen), data=gomez)
anova(mm3) ## identical to averaged model in mm2
summary(mm3)  ## additional variance component, so no information sacrificed


## now look at emmeans for nitro and gen
emmeans(mm3, pairwise~nitro)
emmeans(mm3, pairwise~gen)

## examine pairwise comparisons as compact letter displays (ie. CLD, sometimes called tukey groupings)
## Treatments that share the same number in the ".group" column do not differ significantly, while treatments that
##  with different numbers are significantly different.
##  Oftentimes people use letters rather than numbers
mm3em <- emmeans(mm3, pairwise~nitro)
cld(mm3em)

## extract means and make plot of nitrogen emmeans with CLD
n1 <- emmeans(mm3, ~nitro) %>% as.data.frame()

ggplot(n1, aes(x=nitro, y=emmean)) + geom_point(size=5) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0, lwd=2) + 
  ylab("yield (g) +/- 95% CI") + theme_bw(base_size = 20)+
  annotate("text", x=c(1,2,3,4,5,6), y=7750, label=c("A","B","B","B","B","B"), size=10)


##############################################################################################
##### R CHALLENGE ########################################################
# Do grazing or Nitrogen affect insect abundance?
# 1. Construct an appropriate linear model
# 2. Model assumptions met?
# 3. Do grazing or Nitrogen affect insect abundance?
# 4. How do the results change depending on whether you include a block or split-plot?
# 4a. How big is the blocking and split-plot effect?

d1 <-read_csv("InsectData.csv")
head(d1)
d1$N_Add <- factor(d1$N_Add, levels=c("No.N","Low.N","Med.N","High.N"))  ## reorder factor levels

### check assumptions
ggplot(d1, aes(x=abund)) + 
  geom_histogram(aes(y=..density..), color="white", fill="grey",bins=8)+
  geom_density(alpha=.5, color="red", lwd=1.5) +
  labs(title="histogram of raw data") +
  theme_bw() + theme(text = element_text(size=18))

### plot data
ggplot(d1, aes(y=abund, x=grazed, fill=N_Add))+
  geom_boxplot()+
  geom_point(position = position_jitterdodge(jitter.height=0,jitter.width=.1), 
             size=3, stroke=1.5, pch=21, color="grey")+
  scale_fill_manual(values=c("white","grey90","grey50","grey25"))+    
  theme_bw(base_size = 18)

# Setup model
lm2 <- lmer(abund ~ grazed*N_Add + (1|site)+(1|site:grazed), data=d1)
summary(lm2)
anova(lm2, ddf="Kenward-Roger")

emmeans(lm2, pairwise ~ N_Add) # means and contrasts for Nitrogen addition
emmeans(lm2, pairwise ~ grazed) # means and contrasts for grazing treatment
emmeans(lm2, pairwise ~ N_Add:grazed) # means and contrasts for Nitrogen * grazing treatment

emmeans(lm2, pairwise ~ N_Add|grazed) # means and contrasts Nitrogen sliced by grazing treatment

## extract means and make plot of nitrogen emmeans with CLD
gn1 <- emmeans(lm2, ~N_Add:grazed) %>% as.data.frame()

ggplot() + 
  geom_pointrange(data=gn1, aes(x=grazed, y=emmean, ymin=lower.CL, ymax=upper.CL, color=N_Add),
                  position = position_dodge(width = 1), size=1.25, lwd=1.5) +
  geom_point(data=d1, aes(x=grazed, y=abund, color=N_Add),
                  position = position_jitterdodge(dodge.width = 1), size=2) +
  ylab("Insect abundance +/- 95% CI") + 
  scale_color_viridis(discrete=T, direction=-1, end=.8, option="C")+
  theme_bw(base_size = 20)
