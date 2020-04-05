###############################################
# Project: Visualization of COVID19
# Date: 4/5/2020
# Purpose: Write a function that updates the daily reports from Johns Hopkins university
###############################################

# Setup
gc(reset = TRUE)
rm(list = ls())

#Load Packages
library(tidyverse)


refresh_jhu <- function() {
  if(!file.exists("data/jhu")) dir.create("data/jhu/")
  # Download latest zip file from https://github.com/CSSEGISandData/COVID-19
  download.file("https://github.com/CSSEGISandData/COVID-19/archive/master.zip", "data/jhu/jhu.zip")
  # Unzip latest download
  unzip("data/jhu/jhu.zip", exdir = "data/jhu/files/")
  
  # get list of daily report names
  all_files <- dir("data/jhu/files/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports", full.names = TRUE)
  # keep only csvs
  all_files <- all_files[str_detect(all_files, "\\.csv$")]
  
  # # empty the current folder
  unlink("data/jhu_reports")
  # copy files into current folder
  file.copy(all_files, "data/jhu_reports")
  # Remove the latest download
  unlink("data/jhu/", recursive = TRUE)
  
  # load and zip up files into one R object and output
  # Create a list of read-in files
  read_files <- list.files("data/jhu_reports/", full.names = TRUE) %>% 
    set_names(nm = paste0("file_",basename(.) %>% tools::file_path_sans_ext() %>% str_replace_all("-","_"))) %>% map(read_csv)
  
  # For blog, this is the statement to read them in as independent objects:
  # pmap(.l = list(.x = names(read_files), .y = read_files), .f = ~assign(.x, .y, envir = .GlobalEnv))
  
  saveRDS(read_files, "data/all_jhu.rds")
}





