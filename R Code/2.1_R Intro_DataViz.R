# LOAD Anderson'S IRIS DATA #################
data(iris) # load data (already exists in base R)
head(iris)

# VISUALIZING DATA -- CONTINUOUS VARIABLES #########
## Scatterplots in base R ######

### plot sepal length by width ####
plot(Sepal.Length~Sepal.Width, data=iris)

plot(Sepal.Length~Sepal.Width, data=iris, col="blue")

plot(Sepal.Length~Sepal.Width, data=iris, 
     pch=16, col=c("red","blue","purple")[iris$Species] ) # pch makes solid dots w/ separate color for each species

### plot only the data for Iris virginica ####
plot(Sepal.Length~Sepal.Width, data=iris[iris$Species=='virginica',]) ## use brackets to select the columns you want
                                                                      ## read '==' as 'exactly equals'; one '=' doesnt work here

## plot data and add trendline
plot(Sepal.Length~Sepal.Width, data=iris[iris$Species=='virginica',])
abline(lm(Sepal.Length~Sepal.Width, data=iris[iris$Species=='virginica',]))  ## adds line from lm


## Scatterplots in ggplot2 ########
library(tidyverse) ## install tidyverse if necessary

### plot sepal length by width ####
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length)) +                                               
  geom_point() ## compare to line 14

ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length)) +                                               
  geom_point(color='blue') ## compare to line 18. Coloring all points the same color is done through geom_point()

ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +   #for color-coding by species, it must be called in the aes()                                            
  geom_point() ## compare to line 9

ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +                                            
  geom_point() + facet_wrap(~Species) ## compare to line 20. facet_wrap() separates into different panels

### plot Sepal.Length by Sepal.Width only the data for Iris virginica ####
ggplot(data=iris %>% filter(Species=='virginica'), aes(x=Sepal.Width, y=Sepal.Length)) +                                               
  geom_point()  ## compare to line 14

### plot data and add trendline ####
ggplot(data=iris %>% filter(Species=='virginica'), aes(x=Sepal.Width, y=Sepal.Length)) +                                               
  geom_point() +
  geom_smooth(method='lm')    ## geom_smooth() adds a regression line and method='lm' says use a linear model. Can fit other types of lines.


### add trendline for all three species  ####                                                                 
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) + 
  geom_point() +
  geom_smooth(method='lm')

ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length)) + 
  geom_point(color="blue") +   ## color outside of aes() changes color of all points (ie. not mapped to a column)
  facet_wrap(~Species) +
  geom_smooth(method='lm')


## Make fancy scatterplots #################################
library(viridis) ## install viridis package if necassary

### default plot for Sepal.Length by Sepal.Width for all three species ####
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +          
  geom_point() + geom_smooth(method='lm')

### change theme to new default for better looking plot ####
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +          
  geom_point() + geom_smooth(method='lm') +
  theme_bw()

ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, color=Species)) +          
  geom_point(size=3) + geom_smooth(method='lm') + ## change point size to make them bigger
  scale_color_viridis(discrete=T) +               ## change points to a color-blind friendly palette. Can specify specific colors
  theme_bw()


### same plot as above, but plot species with different shaped points ####
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, shape=Species)) +          
  geom_point(size=3) + geom_smooth(method='lm') +                               
  scale_shape_manual(values=c("circle","square","triangle")) +        ## change shapes. 'shape' must match call in aes()
  theme_bw()

### same plot as above, but plot species by both shape and color ####
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, shape=Species, color=Species)) +          ## color= in aes() changes point shape mapped to a column in iris
  geom_point(size=3) + geom_smooth(method='lm') +                               ## change point size to make them bigger
  scale_color_viridis(discrete=T) +               ## change points to a color-blind friendly palette
  scale_shape_manual(values=c("circle","square","triangle")) +                  ## change shapes
  theme_bw()

### same plot as above, but plot species by both shape and color ####
ggplot(data=iris, aes(x=Sepal.Width, y=Sepal.Length, shape=Species, color=Species)) +          ## color= in aes() changes point shape mapped to a column in iris
  geom_point(size=3) + geom_smooth(method='lm') +                               ## change point size to make them bigger
  scale_color_viridis(discrete=T) +               ## change points to a color-blind friendly palette
  scale_shape_manual(values=c("circle","square","triangle")) +                  ## change shapes
  facet_wrap(~Species) +
  theme_bw(base_size = 14) # increase font size


## Try on your own ##############################################

### Use the mtcars dataset ####
data(mtcars)
?mtcars
head(mtcars)

# 1. Plot x=mpg by y=hp  
# 2. Color code points by wt
# 3. Add trendline. Make the background white. theme_bw is okay, if time try playing around with other themes.
# 4. Facet wrap by cylinders
# 5. Add some regression lines


# VISUALIZING DATA -- CATEGORICAL VARIABLES #########
## Boxplots in base R #############
data(iris) # load data (already exists in base R)
head(iris)
View(iris)

### plot Sepal.Length by Species ####
plot(Sepal.Length~Species, data=iris) #make boxplot

### plot Sepal.Length by Species with color ####
plot(Sepal.Length~Species, data=iris, col=c("red","blue","purple")) #make boxplot with color


## PLOTS FOR CATEGORICAL DATA IN GGPLOT ##########

### boxplot of Sepal.Length by Species ####
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot()

### boxplot w/ points for Sepal.Length by Species ####
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot() + 
  geom_point()

### boxplot w/ jittered points for Sepal.Length by Species ####
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot() + 
  geom_jitter()

### boxplot w/ jittered points for Sepal.Length by Species (fix jittering) ####
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=.15)

### dotplot for Sepal.Length by Species (fix jittering) ####
ggplot(iris, aes(x=Species, y=Sepal.Length)) + 
  geom_dotplot(binaxis = "y", stackdir = "center") 

### violin plot for Sepal.Length by Species ####
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_violin(trim=F)  

### violin plot (w/ dotplot) for Sepal.Length by Species ####
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_violin(trim=F) + 
  geom_dotplot(binaxis = "y", stackdir = "center") 

### violin plot (w/ boxplot) for Sepal.Length by Species ####
ggplot(iris, aes(x=Species, y=Sepal.Length)) + geom_violin(trim=F, bw=.5) + 
  geom_boxplot(width=.1)


## ADDING COLOR AND MEANS TO GGPLOTS #############################

### change color of boxes ####
ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=.15) + 
  scale_fill_viridis(discrete=T)               ## change points to a color-blind friendly palette

### boxplot of Sepal.Length by Species w/ mean ####
ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=.15) + 
  scale_fill_viridis(discrete=T)  +             
  stat_summary(fun=mean, geom="point", size=4, color="red") ## add point for mean

### boxplot of Sepal.Length by Species w/ mean and BW theme ####
ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=.15) + 
  scale_fill_viridis(discrete=T)  +             
  stat_summary(fun=mean, geom="point", size=4, color="red") +
  theme_bw(base_size = 16) 


### setup plot for saving as .tiff file ####
plot1 <- ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species)) +  ## plot now saved as object called 'plot1'
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter(height=0, width=.15) + 
  scale_fill_viridis(discrete=T)  +             
  stat_summary(fun=mean, geom="point", size=3, color="red") +
  theme_bw(base_size = 16) 


### save as .tiff file ####
ggsave("ExamplePlot.tiff", plot1, width=6, height=3, units="in", dpi=300)

## Try on your own Part 2 ###############################################

### Use the mtcars dataset ####
data(mtcars)
?mtcars
head(mtcars)

# 1. Make a boxplot of mpg for the three cyl groups. You should have three boxes. If only one, why? (hint: what type of variable is cyl)
# 2. Change colors, themes, etc.
# 3. Add jittered datapoints on the graph
# 4. ADVANCED: have x-axis cyl and fill boxplots by gear (engine type)


