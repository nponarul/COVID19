###############################################
# Project: COVID19 Exploration
# Date: 5/7/2020
# Purpose: Setup Covid data for Shiny app
###############################################

# Setup
gc(reset = TRUE)
rm(list = ls())

#Load Packages
library(tidyverse)


# LOAD DATA ---------------------------------------------------------------
la_covid <- read_csv("data/la_covid.csv")

# CLEAN DATA --------------------------------------------------------------

la_covid$date <- as.Date(la_covid$date, format = "%m/%d/%Y")
# Set all NAs to 0s
la_covid[is.na(la_covid)] <- 0

# Create a num_new_hosp variable
la_covid <- la_covid %>% 
  mutate(
    new_hosp = hospitalized_ever - lag(hospitalized_ever)
  ) %>%
  replace_na(list(new_hosp = 0))

# save out the cleaned data:
saveRDS(la_covid, "data/la_covid_clean.rds")

