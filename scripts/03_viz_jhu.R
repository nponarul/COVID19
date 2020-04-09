###############################################
# Project: COVID19 Visualization
# Date: 4/8/2020
# Purpose: Visualize COVID 19 JHU data
###############################################

# Setup
gc(reset = TRUE)
rm(list = ls())

#Load Packages
library(tidyverse)

#source function
source("scripts/02_update_jhu.R")

# LOAD DATA ---------------------------------------------------------------

# refresh jhu data
refresh_jhu()

# Load JHU data
jhu <- readRDS("data/all_jhu.rds")

# do field names change? 
jhu %>% map(names) %>% unique() %>% unlist() %>% unique() %>% sort()



# Pull data from New Zealand only -----------------------------------------
# According to CNN, New Zealand has only 1 death so far.
# what are they doing? First case identified on 2/28. Shut borders less than 3 weeks later, closed borders to non-residents. 6 days later nationwide shutdown. What are the cases per capita?
# Shut it's borders on 3/19/2020
# Population in 2018 was 4,885,500  (https://data.worldbank.org/country/new-zealand?view=chart)

# (28/4885500)*100 # Shut down when .06% of the population was infected

# A. Prepare New Zealand data
# 1. Aggregate New Zealand data
nz <- jhu[names(jhu) >= "file_02_28_2020"]
nz <- map(nz, ~.x[.x$country_region == "New Zealand",])
nz <- nz %>% map_df(bind_rows)

# 2. Clean dates

# 3. Plot


# Compare Infection rates with Medicare Disparities -----------------------
# Going to pull data from https://data.cms.gov/mapping-medicare-disparities





