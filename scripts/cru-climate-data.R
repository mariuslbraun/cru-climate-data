# load packages
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(rvest)

# clear workspace
rm(list = ls())

# source functions
source("scripts/functions.R")



# download country climate data ----

# set climate variable for which data should be retrieved
climate_vars = c("tmp", "pre")

for(j in 1:length(climate_vars)) {
  dir.create(file.path("raw", climate_vars[j]), showWarnings = F)

  # get URLs to country files
  link = paste0(
    "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/crucy.2304181636.v4.07/countries/",
    climate_vars[j]
  )
  country_files = get_links(link, ".per")
  country_files$country = str_remove(
    gsub(".*2022.","",country_files$url),
    paste("", climate_vars[j], "per", sep = ".")
  )
  
  # download country files
  download_country_files = function(i) {
    file_path = file.path("raw", climate_vars[j], country_files$url[i])
    if(!file.exists(file_path)) {
      download.file(
        url = file.path(link, country_files$url[i]),
        destfile = file_path
      )
    }
  }
  lapply(1:length(country_files$url), download_country_files)
  
  
  
  # iterate over country list ----
  
  # create data frame name
  df_name = paste(
    climate_vars[j],
    "data",
    sep = "_"
  )
  
  # rbind country data frames and assign to data frame name
  assign(
    x = df_name,
    value = do.call(
      rbind.data.frame, lapply(
      X = 1:length(country_files$url),
      FUN = tidy_climate_var,
      climate_var = climate_vars[j]
      )
    )
  )
  
  # save data frame as RDS file
  saveRDS(
    get(df_name),
    file.path("prepared", paste0(df_name, ".rds"))
  )
  rm(df_name)
}
rm(j)