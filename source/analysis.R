library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

# load packages
library(dplyr)
library(ggplot2)

# Load data set
load_data <- read.csv ("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
load_data
incarceration_data <- data.frame(load_data)
print(incarceration_data)



## Section 2  ---- 
#----------------------------------------------------------------------------#
# How is Incarceration data different between males and females, and how does this change by state? 
#----------------------------------------------------------------------------#

# What is the total number of incarcerations in Washington State by year? 
wa_incarcerations <- incarceration_data %>%
  group_by(year, state) %>%
  filter(state == "WA") %>%
  summarize(sum(total_jail_pop_rate), na.rm = TRUE)
View(wa_incarcerations)

# What are the female incarcerations in Washington by year? 
wa_female_incarcerations <- incarceration_data %>%
  group_by(year, state) %>%
  filter(state == "WA") %>%
  summarize(sum(female_jail_pop), na.rm = TRUE)
  View(wa_female_incarcerations)
  
# What are the male incarcerations in Washington State by year? 
wa_male_incarcerations <- incarceration_data %>%
  group_by(year, state) %>%
  filter(state == "WA") %>%
  summarize(sum(male_jail_pop), na.rm = TRUE)
  View(wa_male_incarcerations)
  
# What is the number of female incarcerations in Florida by year? 
  fl_female_incarcerations <- incarceration_data %>%
  group_by(year, state) %>%
  filter(state == "FL") %>%
  summarize(sum(female_jail_pop), na.rm = TRUE)
  View(fl_female_incarcerations)
  
# What is the number of male incarcerations in Florida by year?
fl_male_incarcerations <- incarceration_data %>%
  group_by(year, state) %>%
  filter(state == "FL") %>%
  summarize(sum(male_jail_pop), na.rm = TRUE)
View(fl_male_incarcerations)

# What is the number of female incarcerations in New York by year?
ny_female_incarcerations <- incarceration_data %>%
  group_by(year, state) %>%
  filter(state == "NY") %>%
  summarize(sum(female_jail_pop), na.rm = TRUE)
View(ny_female_incarcerations)  

# What is the number of male incarcerations in New York by year?
ny_male_incarcerations <- incarceration_data %>%
  group_by(year, state) %>%
  filter(state == "NY") %>%
  summarize(sum(male_jail_pop), na.rm = TRUE)
View(ny_male_incarcerations)


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(maps)

# Create data frame by year
year <- incarceration_data$year
total_prison_pop <- incarceration_data$total_prison_pop

incarceration_df1 <- tibble(year, total_prison_pop) %>% 
  na.omit()
view(incarceration_df1)

# Create function and vector by year
get_year_jail_pop <- function(year) {
  incarceration_df1 %>%
    select(year, total_prison_pop) %>%
    drop_na() %>%
    group_by(year) %>%
    summarise(total_prison_pop = sum(total_prison_pop)) %>%
return()
}


# This function plots the data for total_prison_pop
plot_jail_pop_for_us <- function()  {
  ggplot(get_year_jail_pop(), aes(x = year, y = total_prison_pop)) +
    ggtitle("Population of Prisons (1970-2018)") + 
    labs(x= "Year", y = "U.S. Prison Population", caption = "Data shows total population in prisons") +
    geom_col() 
} 

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
library(tidyverse)

# Create data frame by state
state <- incarceration_data$state
year <- incarceration_data$year
total_prison_pop <- incarceration_data$total_prison_pop

incarceration_df2 <- tibble(year, state, total_prison_pop) %>%
  na.omit()
view(incarceration_df2)

# Create function and vector by state
get_prison_pop_by_states <- function(states) {
  incarceration_df2 %>%
    filter(state %in% c("WA", "OR", "CA")) %>%
    group_by(year) %>%
    mutate(total_prison_pop_by_state = sum(total_prison_pop)) %>%
    select(year, state, total_prison_pop_by_state) %>%
    return()
}

# Make plot
plot_prison_pop_by_states <- function(states) {
  ggplot(get_prison_pop_by_states (states), aes(year, total_prison_pop_by_state, color = factor(state))) +
    ggtitle("Increase of Prison Population by State in the US (1970-2018)") + 
    labs(x = "Year", y = "U.S. Prison Population by State", caption = "Data shows total population in prisons by state") +
    geom_line(aes(color = state))
}

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Variable Comparison By Race In Washington

library(tidyverse)

# Mutate incarceration data for LatinX versus White prison population in Washington State
race_comp_data <- incarceration_data %>%
  group_by(year) %>%
  filter(year >= 1990 && year <= 2010) %>%
  filter(state == "WA") %>%
  summarise(year, latinx_population = latinx_prison_pop, white_population = white_prison_pop, total_population = total_prison_pop)
View(race_comp_data)


# Create continuous chart
race_comparison_chart <- race_comp_data %>%
  pivot_longer(cols = c("latinx_population", "white_population", "total_population"), 
               names_to = "Race", values_to = "value") %>%
  ggplot(mapping = aes(x = Race, y = value, fill = Race)) + geom_point() + 
  labs(x = "year",
       y = "Population", 
       title = "Comparison of Jail Population between Races in Washington (1990-2010)",
       fill = "Population",
       caption = "This graph is meant for viewers to draw a comparison between races in jail population within Washington State")
     
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>

# Load packages
#install.packages("usmap")
library(usmap)
library(ggplot2)

# Minimalist Theme
map_theme <- theme_bw() + 
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

latinx_pop_data <- incarceration_data %>%
  group_by(state) %>%
  filter(year == 2010) %>%
  summarise(latinx_pop = sum(latinx_prison_pop, na.rm = TRUE))

# Create Map LatinX Population in Prison
latinx_pop_data_map <- plot_usmap(
  data = latinx_pop_data, values = "latinx_pop", color = "grey") + map_theme + 
  scale_fill_gradient(low = "#132B43",
                      high = "#56B1F7",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill") + 
  labs(title = "Prison Population of LatinX Individuals in the United States, 2010", 
       fill = "Population", caption = "The data here shows the prison population of LatinX individuals in the United States in 2010")

#----------------------------------------------------------------------------#




