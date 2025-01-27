#### R CHALLENGE 3
library(tidyverse)
library(agridat) ## package with lots of agricultural datasets. Install if needed.
library(viridis)

## Examine the ortiz.tomato.yield dataset. The help gives decent metadata (but it's not great).
## The study grew several tomato genotypes at 18 sites around the world
## There are two datasets, the ortiz.tomato.cov contains site-level information.
## The ortiz.tomato.yield dataset contains data on yield for a bunch of plants grown at each site.
?ortiz.tomato


## Part 1: Examine the yield dataset
data("ortiz.tomato.yield")
yield <- as_tibble(ortiz.tomato.yield)
  
head(yield)
str(yield)
glimpse(yield)
skim(yield) # from skimr package

##################################################################################################################################
## Q1. Look at the structure of the data. How many environments are in the dataset? How many genotypes are in the dataset?




##################################################################################################################################
## Q2. Use mutate() and pipes ( %>% ) to calculate a new variable called 'frt_wt_ha' that is fruit weight per hectare from 
##      2a. yield (number of tomatoes per hectare) and weight (weight per fruit in g). Convert it to kg. 
##      Also, genotype 'OP10' was mislabel. It should be 'OP5'.
##      2b. In the same line of code, use gsub() to replace 'OP10' with 'OP5'.
##      Note that if you rewrite over the original dataset and make a mistake, you can reload it (line 15-16) to clear your errors.



##################################################################################################################################
## Q3. Make a boxplot of each of your three measured variables by environment (yield, weight, and frt_wt_ha).
##     Plot each variable in a separate panel (hint: facet_wrap).
##     Hint: you may need to reshape the data. For the plot, try add scales="free" and nrow=3 to facet_wrap.




## BONUS: Do Q2 and Q3 all in one code block



##################################################################################################################################
## Q4. Make the same plot above but filter out any environment that has 1 for Driv.
##     Hint: You'll need to look in the covs datasets, an associated file, to see which environments have Driving present and use this for filtering.
##     Hint #2: You may need to merge the two datasets into one big one so that you can filter based on Driv.

data("ortiz.tomato.covs")
covs <- as_tibble(ortiz.tomato.covs)

head(covs)
str(covs)
skim(covs)

##     Another hint: you'll need to merge the datasets together in order to filter out the ones that have no Driving.
##     Hint 3: use the full_join command below to merge the datasets. They are relational.

## code to join: replace the DATASET in the code below with the ones you want to merge. 
## If you called your summarized yield dataset something other than 'yield1'. 
##  You can specify the ID column for linking using by='XXXXXX' (but this is not necessary).
tomato <- full_join(DATASET1, DATASET2)





##################################################################################################################################
## Q5. What are the means and standard deviation of 'fruit_ha' for each environment? Use summarize to make a table.
##     Protip: If you run into problems with na's, you can add 'na.rm=T' to your summarize function like this: (frt_wt_ha, na.rm=T). 




## export as .csv file 
?write.csv
write.csv(datasetname, "filenamehere.csv")

