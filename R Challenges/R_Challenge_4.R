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

## examine data structure
head(yield)
str(yield)
glimpse(yield)

#################################################################################
## Q1: Examine the mean, sd, and min/max of yield and weight.
##     Do this for the full dataset and for the two genotypes separately.
## Are the means and medians similar for yield? Does the distribution seem skewed, positively or negatively?
## Are the means and medians similar for weight? Does the distribution seem skewed, positively or negatively?




#################################################################################
## Q2: Examine the distributions for yield and weight (separate ggplots). 
##  Is the distribution skewed?
##  Any potential outliers? If potential outliers, where would you subset the data to exclude them?



## stop here on Tuesday

################################################################################
#### Q3: Merge in covariates from the covs datafile. 
####     Make sure to filter outlier if you decided to remove any.
####     Variables coded as 0/1 will need to be converted to factors using as.factor()
####     Examine relationship using corplot and/or ggpairs()
####     Use na.omit() to omit NA values





## examine variable relationships using ggpairs (HINT: error message is helpful, might need to remove one variable)



################################################################################
#### Q4: Wow that's a lot! 
####      use select() to select just a few variables to view. 
####      Pick ones that seem important and make sure to include 'gen' and 'Driv' plus some continuous variables
####      Examine relationships using corplot and/or ggpairs()

dat2 <- dat1 %>% 
  select(gen, Driv , )

####      Examine relationships using corplot and/or ggpairs()
ggpairs(dat2, aes(color=gen))


################################################################################
################################################################################
#### Q5a: print off a table showing means and sd for the two genotypes and one more categorical variable (so 2 grouping factors)
##        (pick either yield or weight as the response variable)


#### Q5b: make a boxplot showing how one of your response variables (pick either yield or weight) differs by genotype and one more categorical variable.
            ## hint: use x= and color= in the aes()



#### Q5c: make a scatterplot showing how one of your response variables (pick either yield or weight) differs by genotype and one continuous variable.
            ##  hint: use x= and color= in the aes()


