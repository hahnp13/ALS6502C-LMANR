# R code for data exploration ####
library(tidyverse) 
library(agridat)
library(corrplot) #new package
library(EnvStats) #new package
library(skimr)    #new package

# 1.  LOAD AND VIEW Anderson'S IRIS DATA USING BASE R #################
data(iris) # load data (already exists in base R)
iris[8,3] <- 7 # add fake datapoint for demo
head(iris) # print first 6 lines of dataset
tail(iris) # print last 6 lines of dataset

## 1.a glimpse and skim data ####
str(iris) # print 'structure' of dataset giving you info about each column
glimpse(iris) # glimpse is similar to str() in tidyverse
skim(iris)

### 1a.1. third level just for demo ####
# can have more comments here...

# 2. Assess summary statistics and distribution ####

## histogram of petal length
ggplot(iris, aes(x = Petal.Length)) + geom_histogram(bins=12, color="white") + theme_bw(base_size = 16) +
  geom_vline(aes(xintercept= mean(Petal.Length)), color = "blue", size = 2) +
  geom_vline(aes(xintercept= median(Petal.Length)), color = "orange", size = 2)

## histogram using facet_wrap to plot each species in a panel
ggplot(iris, aes(x = Petal.Length)) + geom_histogram(bins=12, color="white") + 
  facet_wrap(~Species, scales="free") + theme_bw(base_size = 16)

## summary to examine mean, median, range
summary(iris)
iris %>% pivot_longer(cols=c(1:4)) %>% group_by(Species,name) %>% summarize(mean=mean(value),median=median(value)) ## table of means/medians by group

# 3. Examine for outliers ####

## boxplot to examine distribution and look for outliers
ggplot(iris, aes(x=Species, y = Petal.Length)) + 
  geom_boxplot(fill="grey", width=.5) + 
  facet_wrap(~Species, scales="free") + 
  theme_bw(base_size = 16)

## test for outlier ####
library(outliers) ## new packages

## grubbs test for outliers #### highest then lowest. Other functions EnvStats::rosnerTest() can test for multiple outliers
grubbs.test(iris$Petal.Length) ## full dataset
grubbs.test(iris$Petal.Length[iris$Species=='setosa']) ## just species setosa
grubbs.test(iris$Petal.Length[iris$Species=='setosa'], opposite=T) ## test lower outlier for species setosa

setos <- iris %>% filter(Species=='setosa')
rosnerTest(setos$Petal.Length)

## remove outlier and remake boxplot. filter with | (OR) will select all observations where one condition is met but not the other. 
iris1 <- iris %>% filter(Petal.Length<4 | !Species=='setosa')

ggplot(iris1, aes(x=Species, y = Petal.Length)) + 
  geom_boxplot(fill="grey", width=.5) + 
  facet_wrap(~Species, scales="free") + 
  theme_bw(base_size = 16)

# 4. Explore relationships among variables ####

# We can first use the GGally package. The ggpairs() code provides us with scatter plots 
#  that plot variables against one another in a pairwise fashion. We also see the distribution of the data
#  and the correlation coefficients between a pair of variables

library(GGally) ## install and load GGally package, if necessary

ggpairs(iris1)   ## Make a big panel plot for exploration!!!
ggpairs(iris1, aes(color=Species, alpha=.75)) ## add color to seperate by species

## alternative to ggpairs(), can be better for quickly scanning complex datasets
iris_cor <- cor(iris1 %>% select(-Species) %>% as.matrix()) ## first make correlation matrix
corrplot(iris_cor, method = "circle", type = "upper") ## plots strength of correlation as color-coded circles

