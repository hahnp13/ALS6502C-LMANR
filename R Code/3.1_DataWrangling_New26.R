#---- Upload all packages ----

# install.packages("palmerpenguins")
# install.packages("janitor")

library(palmerpenguins) ## install if needed
library(tidyverse)
library(janitor) # this package has clean_names 

data(penguins) # this lets you see all of the data sets available in this package
pen <- penguins 

#---- Exploring data ----

print(pen) # will print first 10 rows 
head(pen) # first 6 rows
tail(pen) # last 6 rows 
glimpse(pen) # will focus on columns 

summary(pen)

str(pen) # print 'structure' of dataset giving you info about each column

pen$species # the $ allows you to pull a specific column from a data set 

unique(pen$species) # unique shows you the unique values in that column 

#---- Cleaning up your data ----

## ---- filtering & cleaning names ----

unique(penguins_raw$Species)
unique(penguins_raw$Island)

# filtering 

full_pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  # select lets you keep or remove columns that you want
  filter(Island == "Biscoe") # filter allows you to keep or remove values you'd like from columns 

unique(full_pen$Island) # run this to make sure we only have Biscoe in our islands 

# ticks

full_pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  
  filter(`Clutch Completion` == "No") # if our data has spaces in between words, they will have to always be ticked `` 

unique(full_pen$`Clutch Completion`)

full_pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  # clean_names remves capital letters and spaces from column names, so that the data is easier to work with
  clean_names() %>% 
  filter(clutch_completion == "No") # as you see, now you don't need ticks, and changed it to have an underscore 

## ---- separating double values ---- 

full_pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  
  clean_names() %>% 
  filter(clutch_completion == "No")  %>% 
  separate(species, into = c("common_name", "scientific_name"), # seperate is one way to do this, but you have to watch out that you use // if using a parenthesis
           sep = " \\(") %>% 
  mutate(scientific_name = str_remove(scientific_name, "\\)"))

pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  
  clean_names() %>% 
  filter(clutch_completion == "No")  %>% 
  separate_wider_delim(species, delim = " (", names = c("common_name", "scientific_name")) %>% # seperate_wider_delim is an easier way to do this, more straightforward
  mutate(scientific_name = str_remove(scientific_name, "\\)")) # this line is so that you can fix the left over parenthesis 

# combining two column 

full_pen$common_and_species <- paste(full_pen$common_name, full_pen$scientific_name) # you can create a new column by doing dataset$name_of_new_column <- 

full_pen <- full_pen %>% 
  mutate(common_and_species = paste(common_name, scientific_name)) # or, you can make a new column using mutate. mutate(name_of_new_c = dah dah function)

## ---- adding and modifying variables ----

full_pen <- full_pen %>% 
  mutate(log = log(culmen_length_mm)) # here, we are making a new column that is the log of a variable we chose 

full_pen$log2 <- log(full_pen$culmen_length_mm) # another way to do this, as you've seen above

full_pen$number <- 1:length(full_pen$sample_number) # this adds a column that numbers 1-however many rows you have, hence "length"

# subbing and replacing 

full_pen <- full_pen %>% 
  mutate(sex = sub("MALE", "m", sex)) # one way to sub and replace this, is using "sub". the funciton "gsub" is different, as it will replace EVEYRTHING that contains those characters

full_pen <- full_pen %>% mutate(sex = case_when(sex == "MALE" ~ "m", sex == "FEMALE" ~ "f", TRUE ~ sex)) # If subbing whole variables, my preferred method is case_when 


# using if_else instead: (If sex is "MALE", make it "m", otherwise make it "f")

full_pen <- full_pen %>% 
  mutate(sex = if_else(sex == "MALE", "m", "f")) # this is another way to substitutre things, but works best when you it's binary 

## ----- Groupby ----

full_pen_grouped <- full_pen %>% 
  select(c(island, scientific_name, body_mass_g)) %>% 
  group_by(island) # group by is great, groups by whatever you tell it to. you can do multiple variables

## ---- Summarizing your data ---- #island# ---- Summarizing your data ---- 

# getting the mean, and other values 

full_pen %>% summarize(mean_culmen_depth = mean(culmen_depth_mm, na.rm = T)) # summarize function, may need to use na.rm if have NAs

mean(full_pen$culmen_depth_mm, na.rm = T) # this way, you can just see the mean 

full_pen_grouped <- full_pen_grouped %>% # using summarize in this way, and n() can show you the total number of something, but you have to group it to see it nicely 
  summarize(total = n())

# mean flipper length of each species 

full_pen_flipper <- full_pen %>% 
  select(c(island, scientific_name, flipper_length_mm)) %>% 
  group_by(scientific_name) %>% 
  summarize(mean_flipper_length = mean(flipper_length_mm, na.rm = T)) # this is jusy putting select, group_by, and summ. together. BE CAREFUL WITH THE ORDER YOU PUT THESE IN, R runs by line, so you don't want to override a command that should follow another one

## ---- Reshaping data ---- 

## ---- Reshaping data ---- 

# take the wide penguin data and stack several measurement columns into long format
penguins_long <- full_pen %>%
  pivot_longer(
    cols = c(flipper_length_mm, culmen_length_mm, culmen_depth_mm, body_mass_g), # columns to gather
    names_to = "trait",   # new column for variable names
    values_to = "value")  # new column for their values
  

tail(penguins_long) # look at the last few rows


## now go back to wide format using pivot_wider

penguins_wide_again <- penguins_long %>%
  pivot_wider(
    names_from = trait,   # each trait becomes its own column again
    values_from = value)   # fill columns with the measurement values


head(penguins_wide_again)


## make a small summary table to demonstrate a join

mass_summary <- full_pen %>%
  group_by(scientific_name) %>%
  summarize(mean_mass = mean(body_mass_g, na.rm = TRUE))  # average body mass per species


flipper_summary <- full_pen %>%
  group_by(scientific_name) %>%
  summarize(mean_flipper = mean(flipper_length_mm, na.rm = TRUE))   # average flipper length per species


# join the two summaries using full_join
penguin_means <- full_join(mass_summary,  # first table
                           flipper_summary,   # second table
                           by = "scientific_name")  # key column to match on


penguin_means   # combined table with all species, even if missing in one


## ---- Making graphs ---- 

ggplot(full_pen, aes(x = scientific_name, y = body_mass_g, fill = scientific_name)) +   # base plot with species on x and body mass on y
  geom_boxplot(alpha = 0.8, outlier.shape = 21) +                                      # boxplots with semi transparency and round outliers
  facet_wrap(~island)                                                                 # separate panels by island
scale_fill_viridis(discrete = TRUE, option = "plasma") +                             # colorblind friendly palette
  labs(
    x = "Species",                                                                    # x axis label
    y = "Body Mass (g)",                                                              # y axis label
    title = "Body Mass Variation Across Penguin Species\n(Clutch Completion = No)") + # main title with line break
  theme_bw(base_size = 18) +                                                          # clean black and white theme, larger text
  theme(
    legend.position = "none",                                                         # remove legend
    axis.text.x = element_text(hjust = 0.5, size = 16),                                # center and size x axis text
    axis.text.y = element_text(size = 16),                                            # y axis tick text size
    axis.title = element_text(size = 18),                                             # axis title size
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5))                  # bold, centered, larger title

