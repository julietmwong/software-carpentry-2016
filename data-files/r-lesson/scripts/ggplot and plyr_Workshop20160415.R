# Special topics: ggplot and plyer; Software Carpentry Workshop
# 15Apr16
detach()

#---------------------------
# DATA MANIPULATION - plyer

install.packages('dplyr')
library(dplyr)

dat = read.csv(file.choose())
# Choose mammal_stats.csv
View(dat)
head(dat)
glimpse(dat)
dim(dat)

# To select specific columns: select
select(dat, order, species)
select(dat, order)
# the column species, and any column that starts with "adult"
select(dat, species, starts_with('adult'))
# Selects every row except order
select(dat, -order)

# To select specific rows: filter
# Select out only order that equals "Carnivora"
filter(dat, order == "Carnivora")
# Chain together; select out carnivores, and only small carnivores less than 5kg
filter(dat, order == "Carnivora" & adult_body_mass_g < 5000)
# Select any Carnivores or Primates
filter(dat, order == "Carnivora" | order == "Primates")

# Arrange rows in a datast based on whichever colmn you want: arrange
# Arranges by smallest mammal first
head(arrange(dat, adult_body_mass_g))
# Sort by descending
head(arrange(dat, desc(adult_body_mass_g)))
# Sort first alphabettically by order, then by mass within order
head(arrange(dat, order, adult_body_mass_g))

# What is the smallest mammal?
dat.1 <- arrange(dat, adult_body_mass_g)
head(dat.1)

#Look at data separately by order, and saving it to variable, a
a <- group_by(dat, order)
# Summarize will make a new dataset
# What mean of adult body mass is within data set, a
# For every order, we want to make a new column called mean_mass
# mean_mass will be filled by adult body mass in grams
# na.rm removes any na's from the calculation
b <-summarize(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE))
# mutate let's you keep the same data set but add stuff to the original dataframe
# mutate adds additional columns to the original dataframe
c <-  mutate(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE))
head(c)
glimpse(c)
# Add another function (i.e. max, min, sd) columns
d <-summarize(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE), 
              sd_mass = sd(adult_body_mass_g, na.rm = TRUE))
# if we want to figure otu how the mass of each animal relates to other animals of it's order
# Divide each species' body mass by its order's mean body mass
a <-  group_by(dat, order)
e <- mutate(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE), normalized_mass = adult_body_mass_g / mean_mass)

# We can use Pipes, %>% to push the results of one line to the next
e <- dat%>% # Take our data
  group_by(order) %>% # Split it up by order
  mutate(mean_mass = mean(adult_body_mass_g, na.rm = TRUE)) # Find average weight for every order

# To find the organisms with the biggest mass relative to the rest of its order
# apply the mutate functions, sort by normalized_mass, and only display the species, adult_body_mass_g, and normalized_mass coluns
e <- dat %>%
  group_by(order) %>%
  mutate(mean_mass = mean(adult_body_mass_g, na.rm = TRUE), 
         normalized_mass = adult_body_mass_g / mean_mass) %>%
  arrange(desc(normalized_mass)) %>%
  select(species, normalized_mass, adult_body_mass_g)

# Which order has the most species?
f <- dat %>% # select data
  group_by(order) %>% # pull out order
  summarize(length_species = length(species)) %>% 
  arrange(desc(length_species))

# Which order has the widest range of body mass (max-min)?
g <- dat %>%
  group_by(order) %>%
  summarize(body_mass_range = max(adult_body_mass_g, na.rm = TRUE) - min(adult_body_mass_g, na.rm = TRUE)) %>%
  arrange(desc(body_mass_range))

# Which species of carnivore has the largest body length to body mass ratio?
h <- dat %>%
  filter(order == "Carnivora") %>%
  mutate(mass_ratio = adult_head_body_len_mm, na.rm = TRUE / adult_body_mass_g, na.rm = TRUE) %>%
  arrange(desc(mass_ratio))

# Can also use $ to pull out specific columns
dat$order
dat$adult_head_body_len_mm

# Test pushing to github