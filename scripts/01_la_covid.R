###############################################
# Project: Visualization of COVID 19
# Date: 4/4/2020
# Purpose: Load and process LA data
###############################################

# Setup
gc(reset = TRUE)
rm(list = ls())

#Load Packages
library(tidyverse)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggthemes)

# LOAD DATA ---------------------------------------------------------------
la_covid <- read_csv("data/la_covid.csv")


# UPDATE DATA -------------------------------------------------------------
add_data <- function(date, cases,deaths, hosp, events = NA) {
  return(data.frame(date = date, num_new_cases = cases, num_new_deaths = deaths, hospitalized_ever = hosp, events = events, stringsAsFactors = FALSE))
}
max(la_covid$date) S
la_covid <- bind_rows(
  la_covid 
  # Add new data sets here LAST UPDATED 4/8
  )

write_csv(la_covid, "data/la_covid.csv")
# CLEAN DATA --------------------------------------------------------------

la_covid$date <- as.Date(la_covid$date, format = "%m/%d/%Y")
# Set all NAs to 0s
la_covid[is.na(la_covid)] <- 0
# Derive new 
la_covid <- la_covid %>% filter(date >= as.Date('2020-03-05')) # limit all analysis to start the day we had 10 confirmed cases
# BUILD PLOTS -------------------------------------------------------------

# 1. Plot new cases
p1 <- ggplot(la_covid, aes(date, num_new_cases))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Daily Confirmed Cases in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

# 2. Plot cumulative cases
p2 <- ggplot(la_covid %>% mutate(cum_cases = cumsum(num_new_cases)), aes(date, cum_cases))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Cumulative Cases in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

# 3. Plot new hospitalizations 
p3 <- ggplot(la_covid %>% mutate(new_hosp = hospitalized_ever - lag(hospitalized_ever)), aes(date, new_hosp))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Daily New Hospitalizations in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

# 4. Plot cumulative hospitalizations
p4 <- ggplot(la_covid, aes(date, hospitalized_ever))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Cumulative Hospitalizations in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

# 5. Plot daily new confirmed cases, daily new hospitalizations, and new deaths together
p5_data <- la_covid %>%
  mutate(new_hosp = hospitalized_ever - lag(hospitalized_ever)) %>% 
  select(date, num_new_cases, new_hosp, num_new_deaths) %>% 
  gather(type, value, -date)

p5 <- ggplot(p5_data, aes(x = date, y = value, color = factor(type)))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = brewer.pal(3, "Set1"), labels = c("Hospitalizations", "Cases", "Deaths"), name = "")+
  labs(title = "Daily Confirmed Cases, Hospitalizations and Deaths in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

# 6. Plot cumulative confirmed cases, hospitalizations, and deaths
p6_data <- la_covid %>%
  mutate(cum_deaths = cumsum(num_new_deaths), cum_cases = cumsum(num_new_cases)) %>% 
  select(date, cum_cases, hospitalized_ever, cum_deaths) %>% 
  gather(type, value, -date)

p6 <- ggplot(p6_data, aes(x = date, y = value, color = factor(type)))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = brewer.pal(3, "Set1"), labels = c("Cases", "Deaths", "Hospitalizations"), name = "")+
  labs(title = "Cumulative Cases, Hospitalizations, and Deaths in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

