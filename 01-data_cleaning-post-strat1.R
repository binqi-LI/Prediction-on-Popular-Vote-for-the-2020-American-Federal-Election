#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!

#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/juliazhai/Desktop/PS3")
raw_data <- read_dta("usa_00003.dta")


# Add the labels
raw_data1 <- labelled::to_factor(raw_data)

glimpse(raw_data1)
# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data1 %>% 
  select(sex, 
         age, 
         ftotinc)

         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

  
reduced_data$age <- as.integer(reduced_data$age)

reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)") %>%
  filter(age != "1" & age != "2" & age != "3" & age != "4" & age != "5" & age != "6" & age != "7" & age != "8" & age != "9" & age != "10" & age != "11"& age != "12" & age != "13" & age != "14" & age != "15" & age != "16" & age != "17") %>% 
  filter(ftotinc > 0) %>%
  mutate(ftotinc =
           ifelse( ftotinc <= 14999, "Less than $14,999",
           ifelse( ftotinc %in% 15000:19999, "$15,000 to $19,999",
           ifelse( ftotinc %in% 20000:24999, "$20,000 to $24,999",
           ifelse( ftotinc %in% 25000:29999, "$25,000 to $29,999", 
           ifelse( ftotinc %in% 30000:34999, "$30,000 to $34,999", 
           ifelse( ftotinc %in% 35000:39999, "$35,000 to $39,999",
           ifelse( ftotinc %in% 40000:44999, "$40,000 to $44,999", 
           ifelse( ftotinc %in% 45000:49999, "$45,000 to $49,999",
           ifelse( ftotinc %in% 50000:54999, "$50,000 to $54,999", 
           ifelse( ftotinc %in% 55000:59999, "$55,000 to $59,999", 
           ifelse( ftotinc %in% 60000:64999, "$60,000 to $64,999",
           ifelse( ftotinc %in% 65000:69999, "$65,000 to $69,999", 
           ifelse( ftotinc %in% 70000:74999, "$70,000 to $74,999", 
           ifelse( ftotinc %in% 75000:79999, "$75,000 to $79,999", 
           ifelse( ftotinc %in% 80000:84999, "$80,000 to $84,999",
           ifelse( ftotinc %in% 85000:89999, "$85,000 to $89,999",
           ifelse( ftotinc %in% 90000:94999, "$90,000 to $94,999", 
           ifelse( ftotinc %in% 95000:99999, "$95,000 to $99,999",
           ifelse( ftotinc %in% 100000:124999, "$100,000 to $124,999",
           ifelse( ftotinc %in% 125000:149999, "$125,000 to $149,999", 
           ifelse( ftotinc %in% 150000:174999, "$150,000 to $174,999", 
           ifelse( ftotinc %in% 175000:199999, "$175,000 to $199,999", 
           ifelse( ftotinc %in% 200000:249999, "$200,000 to $249,999", 
           ifelse( ftotinc >= 250000, "$250,000 and above", ftotinc)))))))))))))))))))))))))

reduced_data <- 
  reduced_data %>%
  count(age,sex, ftotinc) %>%
  group_by(age,sex,ftotinc) 

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")



         