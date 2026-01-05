# Begin tutorial for basic R functions ####

555*11        # use R like a calculator
(555*11)+100  # the # blocks code from running

"hello"         # words (characters or factors) need to be in in "quotes"
h1 <- "hello"   # stores "hello" as an object (character)
print(h1)       # prints out the stored object

h2 <- c("hi","hello")   # the 'c' combines values into a list or vector
h2                      # this will also print h2


equation <- (555*11)+100   # saves equation as an object. Object is numeric
1000*equation              # use the stored numeric object in a calculation

## R has many built in functions
ran1 <- rnorm(100, 50, 5)   # 'rnorm' generates random numbers from a normal distribution 
                            ## code generates 100 values with a mean of 50 and sd of 5
                            ## values are saved in object called 'ran1'

?rnorm                      # ? calls for help

length(ran1)    # 'length' is a function that returns how many values are in a column (or vector)
mean(ran1)      # 'mean' is a function that calculates the mean of a vector
sd(ran1)        # 'sd' calculates standard deviation

summary(ran1)

## R can make various plots
plot(ran1)
boxplot(ran1)
hist(ran1, breaks=12)

## binding, etc.
ran2 <- rnorm(100, 10, 2)       # generate new random vector

dataset1 <- cbind(ran1,ran2)    # cbind binds columns
head(dataset1)                  # new dataset has two columns

plot(dataset1)
boxplot(dataset1)
hist(dataset1, breaks=30)       # looks weird


-##############################################-
# LOAD Andersen's IRIS DATA #################
## Data on floral traits for 3 iris species were collected by the botanist Edgar Anderson in 1935

data(iris) # load data (already exists in base R)

mean(iris)               # tries to calculate the mean of the dataset, but errors out. why?
mean(iris$Sepal.Length)  # finds the mean of Sepal.Length within the dataset iris
                         ## the $ selects the column within the dataframe
summary(iris)            # summarizes dataset

head(iris) # print "head" (ie. first 5 lines) of the dataset
iris[1:5,] # manual way to print first 5 lines.
           ## call dataset first, then use brackets select values within [rows, columns] 
           ## : selects everything through 2 values, so [1:5,] selects rows 1:5 with all columns
iris[1:5,1:4] # print first 5 rows but only first 4 columns - [rows,columns]

iris[51,4] # print off whatever is in the 51st row and 5th column

levels(iris$Species)  # the $ selects the column within the dataframe
                      ## 'levels' prints the levels of a factor variable
hist(iris$Sepal.Length) ## make a histogram of sepal length


plot(Sepal.Length~Sepal.Width, data=iris)   # plots X against Y. Read ~ as 'as a function of'
                                            ## 'data=' tells R where to find the data

plot(Sepal.Length~Sepal.Width, data=iris, col="blue")

plot(Sepal.Length~Sepal.Width, data=iris, 
     pch=16, col=c("red","blue","purple")[iris$Species] ) # seperate color for each species

plot(Sepal.Length~Species, data=iris, col=c("red","blue","purple")) #make boxplot

### plot only the data for Iris virginica
plot(Sepal.Length~Sepal.Width, data=iris[iris$Species=='virginica',]) ## use brackets to select the columns you want
                                                                      ## read '==' as 'exactly equals', one '=' dont work here


-##########################################################################-
# SUBSETTING DATA USING TIDYVERSE ####
library(tidyverse)      # load package tidyverse (install if needed)

## SELECT ONLY IRIS VIRGINICA
vir <- filter(iris, Species=="virginica") ## dplyr::filter filters the data
head(vir)

plot(Sepal.Length~Sepal.Width, col="red", data=vir)  ## plot from the subsetted dataframe
plot(Sepal.Length~Sepal.Width, col="red", data=iris[iris$Species=='virginica',])  ## identical to above, 
                                                                                  ## but you don't have to create a new dataframe
plot(Sepal.Length~Sepal.Width, col="red", 
     data=iris %>% filter(Species=="virginica")) ## identical to above using tidyverse
                                                 ## the ' %>% ' is a pipe command, read as 'and then do'
                                                 ## nice bc you don't need to make the subsetted dataset

### what is more intuitive?
    data=iris[iris$Species=='virginica',]  # use iris, within iris select species that are viriginica
    data=iris %>% filter(Species=="virginica") # use iris and then filter to species that are viriginica

hist(vir$Sepal.Length)

### remove plants with small sepal lengths less than 5mm
vir_sl5 <- iris %>% filter(Sepal.Length>5)
head(vir_sl5)  # new head, compare to old
vir_sl5[1:6,]  # different way to look at the head

hist(vir_sl5$Sepal.Length)

###### summarise dataframe
summarise(vir_sl5, mean.sl=mean(Sepal.Length))
summarise(vir_sl5, mean.sl=mean(Sepal.Length), mean.pl=mean(Petal.Length), mean.tl=mean(PetSep.Length))

-################################################################################-
# VISUALIZING DATA in GGPLOT2 ####

ggplot(data=iris) + 
  geom_point(aes(x=Sepal.Width, y=Sepal.Length, color=Species))

ggplot(data=iris) + 
  geom_boxplot(aes(x=Species, y=Sepal.Length, fill=Species), color="black" )

ggplot(data=iris) + 
  geom_point(mapping=aes(x=Sepal.Width, y=Sepal.Length), color="blue") + 
  facet_wrap(~Species)

