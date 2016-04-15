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


#------------------------
# ggplot

# set working directory
getwd()
setwd("~/Desktop/software-carpentry-2016")

# install (if not already) and load package
install.packages("ggplot2")
library(ggplot2)

# Loading mammals dataset for ggplot exercise:
mammals <-  read.csv(file.choose())

# Build a scatter plot of mammal body size and litter size
# Use aes for aesthetics; how to arrange data points (i.e. x and y)
# geom tells what visual representation to put the data in (points, lines, etc.)
myplot <- ggplot(data = mammals, aes(x=adult_body_mass_g, y = adult_head_body_len_mm)) +
  geom_points() +
  scale_x_log10()
myplot
# There are many different geometric objects that you can use (e.g. geom_boxplot(), geom_text())

# Changing the aesthetics of a geom
# Set the size of the data points
myplot <- ggplot(data = mammals, aes(x=adult_body_mass_g, y = adult_head_body_len_mm)) +
  geom_point(size=3) +
  scale_x_log10()
myplot

# See how the orders differ
myplot <- ggplot(data = mammals, aes(x=adult_body_mass_g, y = adult_head_body_len_mm)) +
  geom_point(size=3, aes(color=order)) +
  scale_x_log10()
myplot

# We can set color order to a continuous variable (i.e. liter size)
myplot <- ggplot(data = mammals, aes(x=adult_body_mass_g, y = adult_head_body_len_mm)) +
  geom_point(size=3, aes(color=litter_size)) +
  scale_x_log10()
myplot

# Plot a histogram instead
histogram <- ggplot(data = mammals, aes(x=adult_body_mass_g)) +
  geom_histogram(aes(fill=order)) +
  scale_x_log10()
histogram

# Faceting:
# Facet with respect to a categorical variable
# Panels in which sub-plots are arranged according to a categorical grouping variable(s)
# Make a plot for each order
facet_plot <- ggplot(data = mammals, aes(x=adult_body_mass_g, y = adult_head_body_len_mm)) +
  geom_point(aes(size=litter_size)) +
  scale_x_log10() +
  facet_wrap(~order)
facet_plot

# This code adds to mammals a vector of home range categories
# (we use conditional subsetting to select rows, or individuals, and assign them a factor level in a new vector)
# Create new caegorical variable:
mammals$RangeCategory[mammals$home_range_km2 <= 0.01] <- "micro_machines"
mammals$RangeCategory[mammals$home_range_km2 > 0.01 & mammals$home_range_km2 <= 1] <- "homebodies"
mammals$RangeCategory[mammals$home_range_km2 > 0.1 & mammals$home_range_km2 <= 10] <- "strollers"
mammals$RangeCategory[mammals$home_range_km2 > 10 & mammals$home_range_km2 <= 100] <- "roamers"
mammals$RangeCategory[mammals$home_range_km2 > 100 & mammals$home_range_km2 <= 1000] <- "free_agents"
mammals$RangeCategory[mammals$home_range_km2 > 1000] <- "transcendentalists"

head(mammals$RangeCategory)

# Use filter to create a subset of our mammals data that only includes a few orders
# We tell R to put the factor levels in RangeCategory and order in order
# Now they will plot from small to large
# We are not changing the order of rows in our data.frame
OrderSubset<-filter(mammals, order == "Rodentia" | order == "Cetacea" | order=="Primates" | order=="Carnivora") 
OrderSubset$RangeCategory <- factor(OrderSubset$RangeCategory, levels = c("micro_machines", "homebodies", "strollers", "roamers", "free_agents", "transcendentalists"))
OrderSubset$order <- factor(OrderSubset$order, levels = c("Rodentia", "Carnivora", "Primates", "Cetacea"))

# Now we make a plot of our new dataset
Facet_fig <- ggplot(data = OrderSubset, aes(x=adult_body_mass_g)) +
  geom_histogram(aes(fill=order)) +
  scale_x_log10() +
  facet_grid(RangeCategory~order, scales = "free") # display facets with RangeCategory horizontally, and order vertically
Facet_fig

# Make a figure that summarizes body sizes with respect to the range category, and separately for a few different orders?
mammals_boxplot <- ggplot(data = OrderSubset, aes(y = adult_body_mass_g, x=order)) +
  geom_boxplot(aes(color=order)) +
  scale_y_log10() +
  facet_grid(RangeCategory~.)
mammals_boxplot

# Use argument, "stats"
# you end up with some summarization of your data
ggplot(data=OrderSubset, aes(x=adult_body_mass_g, y=litter_size)) +
  geom_point(aes(color=order)) +
  scale_x_log10() +
  facet_grid(RangeCategory~order, scales="free")
  geom_smooth(method=lm)

# Controlling figure appearance - theme    
# You can customize your own
ggplot(data=OrderSubset, aes(x=adult_body_mass_g)) +
  geom_histogram(aes(fill=order)) +
  scale_x_log10() +
  facet_grid(RangeCategory~order, scales="free") +
  theme_bw() +
  theme(legend.key = element_rect(fill=NA),
        legend.position = "bottom",
        axis.title=element_text(angle=0, size=18, face="bold"),
        legend.text=element_text(angle=0, size=12, face="bold"),
        panel.background=element_rect(fill=NA))

# install wesanderson package
install.packages("wesanderson")
library(wesanderson)

# to see color palettes
wes_palette("Royal1")
wes_palette("FantasticFox")
wes_palette("Zissou")
wes_palette("GrandBudapest")

ggplot(data=OrderSubset, aes(x=adult_body_mass_g)) +
  geom_histogram(aes(fill=order)) +
  scale_x_log10() +
  facet_grid(RangeCategory~order, scales="free") +
  scale_fill_manual(values=wes_palette("GrandBudapest2"))
  theme_bw() +
  theme(legend.key = element_rect(fill=NA),
        legend.position = "bottom",
        axis.title=element_text(angle=0, size=18, face="bold"),
        legend.text=element_text(angle=0, size=12, face="bold"),
        panel.background=element_rect(fill=NA))