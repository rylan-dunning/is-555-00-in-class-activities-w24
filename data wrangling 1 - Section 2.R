library(tidyverse)

# Because `starwars` is a "hidden" dataset in memory for demonstration, it won't show up in our environment at 
# first, and it'll also be hard to reset it if we make a mistake. So assign it to `df` to make sure you can work
# with it.
df <- starwars

# glimpse turns a data frame on its side. Super useful.
df %>%
  glimpse()
  


# iteratively add operations: 
# height > 100, sex == female, 
# choose name, height, mass, species, films, 
# mass > 50, 
# arrange by mass
# note: filtering on some logical excludes NAs
tall_females <- df %>% 
  filter(height > 100,
         sex == 'female') %>% 
  select(name, height, mass, species, films) %>% 
  filter(mass > 50) %>% 
  arrange(mass)

# calculate a new column,weight_lbs = mass * 2.204623
# Make sure it gets saved to the tibble...
df <- df %>% 
  mutate(weight_lbs = mass*2.204623) 


# group and summarize. Pay attention to NAs
# get a count and mean mass by species
df %>%
  group_by(species) %>% 
  # filter(!is.na(mass)) %>% 
  summarize(species_count = n(),
            na_count = sum(is.na(mass)),
            avg_mass = mean(mass, na.rm = TRUE)) 

df %>% 
  filter(species == 'Chagrian')


df %>% 
  arrange(desc(height)) %>% 
  slice_tail(n=3)

df %>% 
  slice_min(height, n = 3)

df %>% 
  slice_min(eye_color, n = 3)

df %>% 
  slice_min(mass, n = 4) #getting back 5 rather than 4

df %>% 
  slice_sample(n=10) #slice_sample is random

df %>% 
  slice_sample(prop = .1) #10 percent of the data



# Lots of NAs for average... why? Even for large groups it's NA...


# Humans definitely have SOME mass data...


# so let's exclude NAs from the mean aggregation:



# why does Chagrian have a weird NaN?


# top 5 tallest overall - using slice_head


# or just using slice_max


# what is the shortest character for each species? 

df

df %>% 
  group_by(species) %>% 
  slice_max(height)

df %>% 
  group_by(species) %>% 
  slice_min(height, with_ties = F)

df %>% filter(height == 150)

df %>% 
  count(height, sort = T)

df %>% 
  group_by(species) %>% 
  slice_max(species_count = n())




# ALso summarize isn't always the goal what if we want a relative 
# comparison of height for each species? I.e., are you tall for a human?
# calculate tallest for each species without summarizing, convert to percentile, show a few of the columns, sort





# Grouping by multiple
# Was is the average birth year for each gender from each homeworld

df %>% 
  group_by(homeworld, gender) %>% 
  summarize(count = n(),
            avg_birth_year = mean(birth_year, na.rm = T))

df %>% 
  group_by(species) %>% 
  slice_min(mass, n = 1, with_ties = F) %>% 
  group_by(sex) %>% 
  summarize(avg_height = mean(height, na.rm = T))




