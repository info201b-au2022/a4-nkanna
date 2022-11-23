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
print(df1)



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
 
get_year_jail_pop <- function() {
  jurisdiction_level_data %>%
    select(yjid, total_jail_pop)
return()   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  return()   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


