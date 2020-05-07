###############################################
# Project: COVID 19 Exploration
# Date: 5/7/2019
# Purpose: Create Shiny functions 
###############################################

# Setup
gc(reset = TRUE)
rm(list = ls())

#Load Packages
library(tidyverse)


# FUNCTIONS ---------------------------------------------------------------

# Subset the dataset to a specific date range
subset_data <- function(df, date1, date2, date_var = "date") {
  return(df %>% filter(between(!!sym(date_var), date1, date2)))
}

# function will return a n-day average of a given variable
n_day_average <- function(df, col, n, date_var) {
  # get subset of fields
  tmp <- df[, c(col, date_var)]
  
  # create intervals
  tmp <- tmp %>% 
    mutate(
      id = row_number(), # add an id
      intev = ifelse(id %% n == 0, 1, 0), # flag beginning of interval
      intev = cumsum(intev) # turn this into an interval ID
    )
  
  # get n-day average
  var <- paste0(col, "_", n,"_day_avg")
  
  tmp <- tmp %>% 
    group_by(intev) %>% 
    mutate(
      !!sym(var) := sum(!!sym(col))/n()
    ) %>% ungroup()
  
  # return object with date, original variable, and average variable
  return(tmp %>% select(-id, -intev))
  
  
}

get_cumul <- function(df, col, date_var){
  # get subset of fields
  tmp <- df[, c(col, date_var)]
  
  # create a cumuluative field
  var = paste0(col, "_cumul")
  
  tmp <- tmp %>% 
    mutate(
      !!sym(var) := cumsum(!!sym(col))
    )
  
  return(tmp)
}
