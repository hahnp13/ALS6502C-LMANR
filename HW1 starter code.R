library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(glmmTMB)
library(performance)
library(MuMIn)
library(tidyverse)
## load / install more as necassary

# Step 1: Load and explore data ####
## load in csv file. Make sure your working directory is set to folder that contains the data
## Note that if you open the R file from the folder that contains the data, your working directory will automatically be set to that folder.
## You may load additional packages, as needed.
hw1 <-read_csv("ALS6502C HW1 - Sheet1.csv")
head(hw1)
glimpse(hw1) # check to make sure everything looks good

## plot out data to check
ggplot(hw1, aes(x=Hand, y=Time_sec, color=Type))+
  geom_boxplot()
unique(hw1$Type)

## wrap by person to further check
ggplot(data=hw1, aes(x=Hand, y=Time_sec, fill=Type)) + geom_boxplot(outlier.shape = NA) + 
  geom_point(pch=21, size=2, stroke=1.5, position=position_jitterdodge(jitter.width = .2))+
  facet_wrap(~Person) ## might be better to not facet_wrap by person here, or try both

# check to make sure each person has data for both hands and both types
hw1 %>%
  distinct(Person, Hand, Type) %>%
  count(Person) %>%
  arrange(n) %>% 
  print (n = 30)
