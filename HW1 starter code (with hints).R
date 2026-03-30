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

ggplot(hw1, aes(x=Hand, y=Time_sec, color=Type))+
  geom_boxplot()
unique(hw1$Type)

## AFTER EXPLORING THE DATASET, BELOW IS CODE YOU CAN USE TO MODIFY TO THE LEVEL NAMES OF FACTORS (if that is something you think you might want/need to do)
## obviously you will replace VARIABLE, NEWLEVELNAME and ORIGINALNAME with the appropriate names
hw1 <- hw1 %>% 
  mutate(Type = fct_recode(as.factor(Type), "NEWLEVELNAME" = "ORIGINALNAME"))

ggplot(hw1, aes(x=Hand, y=Time_sec, color=Type))+
  geom_boxplot()

## Another way of exploring the data to check for data entry errors
## If someone's data wasn't property entered, for example they don't properly have all treatments, you may want to filter that out
ggplot(data=hw1, aes(x=Hand, y=Time_sec, fill=Type)) + geom_boxplot(outlier.shape = NA) + 
  geom_point(pch=21, size=2, stroke=1.5, position=position_jitterdodge(jitter.width = .2))+
  facet_wrap(~Person) ## might be better to not facet_wrap by person here, or try both

# check to make sure each person has full data for both hands and both types (n should = 4 for each person)
hw1 %>%
  distinct(Person, Hand, Type) %>%
  count(Person) %>%
  arrange(n) %>% 
  print (n = 30)
