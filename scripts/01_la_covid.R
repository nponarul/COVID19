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


# CLEAN DATA --------------------------------------------------------------

la_covid$date <- as.Date(la_covid$date, format = "%m/%d/%Y")

# Derive new 

# BUILD PLOTS -------------------------------------------------------------

# 1. Plot new cases
p1 <- ggplot(la_covid, aes(date, num_new_cases))+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Confirmed Cases in LA County")+
  xlab("Date")+
  ylab("Number of Cases")+
  ggthemes::theme_tufte()

