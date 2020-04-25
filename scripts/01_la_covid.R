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
max(as.Date(la_covid$date, format = "%m/%d/%Y")) 
la_covid <- bind_rows(
  la_covid, 
  # Add new data sets here LAST UPDATED 4/23
  # add_data("04/24/2020", 1035,52, 4211 ),
  # add_data("04/25/2020", 607, 48, 4319 )
  )

write_csv(la_covid %>% distinct(), "data/la_covid.csv")
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

# 7. Plot a 3 day moving average for each
# first date is march 5. Graph will start on March 8 and have 3 day lookback average
p7_data <- la_covid %>%
  mutate(new_hosp = hospitalized_ever - lag(hospitalized_ever)) %>%
  replace_na(list(new_hosp = 0)) %>% 
  mutate(avg_3_cases = ifelse(date >= as.Date("2020-03-08"), (num_new_cases + lag(num_new_cases, 1) + lag(num_new_cases, 2))/3, 0),
         avg_3_hosp = ifelse(date >= as.Date("2020-03-08"), (new_hosp + lag(new_hosp, 1) + lag(new_hosp, 2) + lag(new_hosp,3))/4, 0),
         avg_3_deaths = ifelse(date >= as.Date("2020-03-08"), (num_new_deaths + lag(num_new_deaths, 1) + lag(num_new_deaths, 2) + lag(num_new_deaths, 3))/4, 0)) %>% 
  select(date, avg_3_hosp, avg_3_deaths) %>% 
  gather(type, value, -date)

p7 <- ggplot(p7_data %>% filter(date >= as.Date("2020-03-08")), aes(x = date, y = value, color = factor(type, levels = c( "avg_3_hosp", "avg_3_deaths"))))+
  geom_line()+
  scale_x_date(breaks = seq(as.Date("2020-03-08"), as.Date("2020-04-23"), by = 3))+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = brewer.pal(3, "Set1"), labels = c( "Hospitalizations", "Deaths"), name = "")+
  labs(title = "3-day Average of Daily Hospitalizations, and Deaths in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

# 8. Plot a percentage increase in hospitalizations (date of first hospitalization is 3/21)
p8_data <- la_covid %>% 
  mutate(
    rate_hosp_diff = ifelse(hospitalized_ever == 0 & lag(hospitalized_ever,3) == 0, 0, (hospitalized_ever-lag(hospitalized_ever, 3))/hospitalized_ever) 
  )

p8 <- ggplot(p8_data %>% filter(date >= as.Date("2020-03-21")), aes(x = date, y = rate_hosp_diff))+
  geom_line()+
  scale_x_date(breaks = seq(as.Date("2020-03-21"), as.Date("2020-04-25"), by = 3))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = .1))+
  # scale_color_manual(values = brewer.pal(3, "Set1"), labels = c( "Hospitalizations", "Deaths"), name = "")+
  labs(title = "Percent Change in New Hospitalizations in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

# 9. Plot the percentage change in cases 
p9_data <- la_covid %>% 
  mutate(
    cum_new_cases = cumsum(num_new_cases),
    rate_cases_diff = ifelse(cum_new_cases == 0 & lag(cum_new_cases,3) == 0, 0, (cum_new_cases-lag(cum_new_cases,3))/cum_new_cases)
  )

p9 <- ggplot(p9_data %>% filter(date >= as.Date("2020-03-08")), aes(x = date, y = rate_cases_diff))+
  geom_line()+
  scale_x_date(breaks = seq(as.Date("2020-03-08"), as.Date("2020-04-25"), by = 3))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = .1))+
  # scale_color_manual(values = brewer.pal(3, "Set1"), labels = c( "Hospitalizations", "Deaths"), name = "")+
  labs(title = "Percent Change in New Cases in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()


# 9. Plot the percentage change in deaths 
p10_data <- la_covid %>% 
  mutate(
    cum_new_deaths = cumsum(num_new_deaths),
    rate_deaths_diff = ifelse(cum_new_deaths == 0 & lag(cum_new_deaths,3) == 0, 0, (cum_new_deaths-lag(cum_new_deaths,3))/cum_new_deaths)
  )

p10 <- ggplot(p10_data %>% filter(date >= as.Date("2020-03-19")), aes(x = date, y = rate_deaths_diff))+
  geom_line()+
  scale_x_date(breaks = seq(as.Date("2020-03-19"), as.Date("2020-04-25"), by = 3))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = .1))+
  # scale_color_manual(values = brewer.pal(3, "Set1"), labels = c( "Hospitalizations", "Deaths"), name = "")+
  labs(title = "Percent Change in New Deaths in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

