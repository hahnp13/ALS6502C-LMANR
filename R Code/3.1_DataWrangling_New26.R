#---- Upload all packages ----
# install.packages("palmerpenguins")
library(palmerpenguins) ## install if needed
library(tidyverse)
library(janitor)

data(penguins)
pen <- penguins

#---- Exploring data ----

print(pen)
head(pen)
tail(pen)
glimpse(pen)

summary(pen)

str(pen) # print 'structure' of dataset giving you info about each column

pen$species

unique(pen$species)

#---- Cleaning up your data ----

## ---- filtering & cleaning names ----

penguins_raw %>% print()
penguins_raw %>% glimpse()

unique(penguins_raw$Species)
unique(penguins_raw$Island)

# filtering 

full_pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  
  filter(Island == "Biscoe") 

unique(full_pen$Island)

# ticks

full_pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  
  filter(`Clutch Completion` == "No") 

unique(full_pen$`Clutch Completion`)

full_pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  
  clean_names() %>% 
  filter(clutch_completion == "No") 

## ---- separating double values ---- 

full_pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  
  clean_names() %>% 
  filter(clutch_completion == "No")  %>% 
  separate(species, into = c("common_name", "scientific_name"),
           sep = " \\(") %>% 
  mutate(scientific_name = str_remove(scientific_name, "\\)"))

pen <- penguins_raw %>% 
  select(-c(studyName,Comments)) %>%  
  clean_names() %>% 
  filter(clutch_completion == "No")  %>% 
  separate_wider_delim(species, delim = " (",
                       names = c("common_name", "scientific_name")) %>% 
  mutate(scientific_name = str_remove(scientific_name, "\\)"))

# combining two column 

full_pen$common_and_species <- paste(full_pen$common_name, full_pen$scientific_name)

full_pen <- full_pen %>% 
  mutate(common_and_species = paste(common_name, scientific_name))

## ---- adding and modifying variables ----

full_pen <- full_pen %>% 
  mutate(log = log(culmen_length_mm))

full_pen$log2 <- log(full_pen$culmen_length_mm)

full_pen$number <- 1:length(full_pen$sample_number)

# subbing and replacing 

full_pen <- full_pen %>% 
  mutate(sex = sub("MALE", "m", sex))

full_pen <- full_pen %>% mutate(sex = case_when(
  sex == "MALE" ~ "m",
  sex == "FEMALE" ~ "f",
  TRUE ~ sex))

# using if_else instead: (If sex is "MALE", make it "m", otherwise make it "f")

full_pen <- full_pen %>% 
  mutate(sex = if_else(sex == "MALE", "m", "f"))

## ----- Groupby ----

full_pen_grouped <- full_pen %>% 
  select(c(island, scientific_name, body_mass_g)) %>% 
  group_by(island)

## ---- Summarizing your data ---- #island# ---- Summarizing your data ---- 

# getting the mean, and other values 

full_pen %>% summarize(mean_culmen_depth = mean(culmen_depth_mm, na.rm = T))

mean(full_pen$culmen_depth_mm, na.rm = T)

full_pen_grouped <- full_pen_grouped %>% 
  summarize(total = n())

# mean flipper length of each species 

full_pen_flipper <- full_pen %>% 
  select(c(island, scientific_name, flipper_length_mm)) %>% 
  group_by(scientific_name) %>% 
  summarize(mean_flipper_length = mean(flipper_length_mm, na.rm = T))

## ---- Reshaping data ---- 

penguins_long <- full_pen %>%
  pivot_longer(
    cols = c(flipper_length_mm, culmen_length_mm, culmen_depth_mm, body_mass_g),
    names_to = "Trait",
    values_to = "value"
  )

tail(penguins_long)

library(ggplot2)
library(viridis)

ggplot(full_pen, aes(x = scientific_name, y = body_mass_g, fill = scientific_name)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(
    x = "Species",
    y = "Body Mass (g)",
    title = "Body Mass Variation Across Penguin Species\n(Clutch Completion = No)") +
  theme_bw(base_size = 18) +   # makes everything bigger
  theme(
    legend.position = "none",
    axis.text.x = element_text(hjust = 0.5, size = 16),  
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5))

