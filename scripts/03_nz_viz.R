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
library(scales)
library(RColorBrewer)
#source function
source("scripts/02_update_jhu.R")

# LOAD DATA ---------------------------------------------------------------

# refresh jhu data
# refresh_jhu()

# Load JHU data
jhu <- readRDS("data/all_jhu.rds")

# do field names change? 
# jhu %>% map(names) %>% unique() %>% unlist() %>% unique() %>% sort()



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

# 2. Clean dates and numbers
# there are a couple with a different format
nz$last_update[nz$last_update == "3/22/20 23:45"] <- "2020-03-22T23:45"
nz$last_update[nz$last_update == "4/6/20 23:21"] <- "2020-04-06T23:21"

nz$last_update <- as.Date(as.POSIXct(nz$last_update))

nz <- nz %>% 
  mutate_at(c("confirmed", "recovered", "deaths"), as.numeric)

# 3. Plot
# a. Plot new cases, recoveries, and deaths
# first, replace all mising values with 0
nz[is.na(nz)] <- 0
nz_daily_data <- nz %>%
  mutate(new_cases = confirmed - lag(confirmed),
         new_recov = recovered - lag(recovered),
         new_deaths = deaths - lag(deaths),
         #make sure we have first case
         new_cases = ifelse(last_update == as.Date("2020-02-28"), 1, new_cases)
         ) %>% 
  select(last_update, new_cases, new_recov, new_deaths) %>% 
  gather(type, value, -last_update) %>% distinct()

nz_daily <- ggplot(nz_daily_data, aes(x = last_update, y = value, color = factor(type, levels = c("new_cases", "new_recov", "new_deaths"))))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = brewer.pal(3, "Set1"), labels = c("Cases", "Recoveries", "Deaths"), name = "")+
  labs(title = "Daily Confirmed Cases, Recoveries\nand Deaths in New Zealand")+
  xlab("Date")+
  ylab("Number of Individuals")+
  ggthemes::theme_tufte()

ggsave("data/nz_daily.png", nz_daily, width = 5, height = 5)

# b. Plot cumulative cases, recoveries, and deaths
nz_cum <- nz %>% 
  select(last_update, confirmed, recovered, deaths) %>% 
  gather(type, value, -last_update) %>% distinct()

nz_cum_plot <- ggplot(nz_cum, aes(x = last_update, y = value, color = factor(type, levels = c("confirmed", "recovered", "deaths"))))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = brewer.pal(3, "Set1"), labels = c("Cases", "Recoveries", "Deaths"), name = "")+
  labs(title = "Cumulative Confirmed Cases, Recoveries\nand Deaths in New Zealand")+
  xlab("Date")+
  ylab("Number of Individuals")+
  ggthemes::theme_tufte()

ggsave("data/nz_cum.png", nz_cum_plot, width = 5, height = 5)


# b. Plot cumulative cases, recoveries, and deaths, as % of total population
nz_cum_plot_perc <- ggplot(nz_cum, aes(x = last_update, y = value/4885500, color = factor(type, levels = c("confirmed", "recovered", "deaths"))))+
  geom_line()+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = brewer.pal(3, "Set1"), labels = c("Cases", "Recoveries", "Deaths"), name = "")+
  labs(title = "Cumulative Confirmed Cases, Recoveries\nand Deaths in New Zealand")+
  xlab("Date")+
  ylab("Number of Individuals")+
  ggthemes::theme_tufte()

ggsave("data/nz_cum_perc.png", nz_cum_plot_perc, width = 5, height = 5)

# Compare Infection rates with Medicare Disparities -----------------------
# Going to pull data from https://data.cms.gov/mapping-medicare-disparities





