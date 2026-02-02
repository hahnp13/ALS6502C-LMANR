#### R CHALLENGE 4
library(tidyverse)
library(agridat) 
library(GGally)

## Examine the ortiz.tomato.yield dataset. The help gives decent metadata (but it's not great).
## The study grew several tomato genotypes at 18 sites around the world
## There are two datasets, the ortiz.tomato.cov contains site-level information.
## The ortiz.tomato.yield dataset contains data on yield for a bunch of plants grown at each site.
?ortiz.tomato

## Preparation: merge yield and covs dataset and select variables of interest
data("ortiz.tomato.yield")
data("ortiz.tomato.covs")

## today we will work with two genotypes (OP15 and OP3) rather than all 15 genos.
yield <- ortiz.tomato.yield %>% filter(gen=='OP15'|gen=='OP3') %>% droplevels()  ## filter data. 
            ## Need to use 'droplevels()' otherwise it "remembers" the levels that are filtered out. Sometimes it is problematic.

################################################################################
## First, examine data structure. We will focus on the response variable "yield"
head(yield)
str(yield)
glimpse(yield)

#################################################################################
# Q1. Check for 1) outliers and 3) distribution (ie. normality) in the response variable of "yield"  ####

## Examine plots to visually assess for outliers and distribution, plot by 

## Look at median, means, etc. to check for skew

### skim and skim by group (or summarize by group)


################################################################################
# Q2. Examine potential covariates for potential colinearity ####
dat1 <- full_join(ortiz.tomato.covs,yield,by="env") %>% 
  mutate(gen=as.factor(gen), Driv=as.factor(Driv), Trim=as.factor(Trim), Irr=as.factor(Irr))

### Any potential problematic correlations?
### Hint: use ggpairs(), may need to remove a problematic column


## also try cor and corrplot


### Focus just on a few covariates
dat2 <- dat1 %>% 
  select(gen, Driv, Dha,MnT,Prec,yield)


################################################################################
# Q3. Examine relationship between yield and potential covariates ####
## print off a table showing means and sd for the two genotypes and one more categorical variable (so 2 grouping factors; use Driv as the other grouping factor)

#### make a boxplot showing how yield differs by genotype and one more categorical variable (Driv).
## hint: use x= and color= in the aes()

ggplot(dat2, aes(x=gen, y=weight, color=as.factor(Driv))) + geom_boxplot()

#### make a scatterplot showing how one of your response variables (pick either yield or weight) differs by genotype and one continuous variable.
##  hint: use x= and color= in the aes()
