#### R CHALLENGE 4
library(tidyverse)
library(agridat) 
library(GGally)
library(skimr)
library(corrplot)

## Examine the ortiz.tomato.yield dataset. The help gives decent metadata (but it's not great).
## The study grew several tomato genotypes at 18 sites around the world
## There are two datasets, the ortiz.tomato.cov contains site-level information.
## The ortiz.tomato.yield dataset contains data on yield for a bunch of plants grown at each site.
?ortiz.tomato

## Preparation: load in the yield and covs dataset 
data("ortiz.tomato.yield")
data("ortiz.tomato.covs")

# Q1: in one code chunk, do the following: ###########################################
## merge together the two data frames
## today we will work with two genotypes (OP15 and OP3) rather than all 15 genos
## convert the following variables to factors: gen, Driv, Trim, Irr
## call the new dataset 'tom1'

# Q2: examine data structure and view the dataframe. We will focus on the response variable "yield" ####


# Q3. Check for 1) outliers, 2) variance/dispersion, and 3) distribution (ie. normality) in the response variable of "yield"  ####

## Examine plots to visually assess for outliers and distribution, plot by env

## Look at median, means, etc. to check for skew (be careful with normality and outlier tests)

### skim and skim by group (optional)


# Q4. Examine potential covariates for potential colinearity ####
### Hint: use ggpairs(), may need to remove columns that are factors
### Any very high correlations?


### Focus just on a few covariates
 #### select(gen, Driv, Dha,MnT,Prec,yield)
### color by gen to look at each genotype seperately

# Q5. Examine relationship between yield and potential covariates ####
## print off a table showing means and sd for the two genotypes and one more categorical variable (so 2 grouping factors; use Driv as the other grouping factor)

#### make a boxplot showing how yield differs by genotype and one more categorical variable (Driv).
## hint: use x= and color= in the aes()

ggplot(dat2, aes(x=gen, y=yield, color=as.factor(Driv))) + geom_boxplot()

#### make a scatterplot showing how one of your response variables (pick either yield or weight) differs by genotype and one continuous variable.
##  hint: use x= and color= in the aes()
