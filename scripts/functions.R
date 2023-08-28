# load packages
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(rvest)
library(sf)

# retrieve all URLs from website that match pattern ----
get_links = function(link, pattern) {
  links = read_html(
    link
  ) %>% html_elements("a") %>%
    html_attr("href")
  links = as.data.frame(links) %>%
    filter(
      str_detect(links, pattern)
    )
  colnames(links) = "url"
  return(links)
}

# clean up country climate data ----
tidy_climate_var = function(i, climate_var) {
  # load tab delimited country file
  country_data = read_delim(
    file.path(
      "raw",
      climate_var,
      country_files$url[i]
    ),
    skip = 2,
    col_names = T,
    delim = "\t",
    trim_ws = T,
    show_col_types = F
  )
  
  # change column 
  colnames(country_data) = "values"
  
  # clean up delimitation to split up into month and year columns
  country_data$values = str_replace_all(country_data$values, "    ", ",")
  country_data$values = str_replace_all(country_data$values, " ", "")
  country_data = str_split(country_data$values, ",")
  
  # rbind vectors together to data frame
  # (remove last year because data for some months missing)
  country_data = do.call(
    rbind.data.frame,
    country_data[-length(country_data)]
  )
  
  # set column and row names
  colnames(country_data) = country_data[1, ]
  rownames(country_data) = country_data[, 1]
  
  # drop season columns
  country_data = country_data %>% select(-c(MAM, JJA, SON, DJF)) %>%
    filter(row.names(country_data) != "YEAR")
  
  # pivot data to long format
  country_data_monthly = country_data %>%
    select(-ANN) %>%
    pivot_longer(
      cols = JAN:DEC,
      names_to = c("year", "month"),
      names_sep = "_",
      values_to = climate_var
    ) %>%
    select(-month)
  colnames(country_data_monthly) = c("year", "month", climate_var)
  country_data_monthly$country = country_files$country[i]
  
  # convert types of variables
  country_data_monthly$year = as.numeric(country_data_monthly$year)
  country_data_monthly$month = as.factor(country_data_monthly$month)
  country_data_monthly[climate_var][, 1] = as.numeric(
    unlist(country_data_monthly[climate_var][, 1])
  )
  country_data_monthly[country_data_monthly == -999.0] = NA
  country_data_monthly$country = as.factor(country_data_monthly$country)
  
  # calculate anomaly relative to 1901-2000 average
  country_mean_1901_2000 = mean(
    as.numeric(unlist((country_data_monthly %>%
                         filter(year >= 1901 & year <= 2000)
    )[climate_var][, 1])), na.rm = T
  )
  country_sd_1901_2000 = sd(
    as.numeric(unlist((country_data_monthly %>%
                         filter(year >= 1901 & year <= 2000)
    )[climate_var][, 1])), na.rm = T
  )
  country_data_monthly$anom_1901_2000 = (
    as.numeric(unlist(country_data_monthly[climate_var][, 1])) -
      country_mean_1901_2000
  ) / country_sd_1901_2000
  
  # return data frame
  return(country_data_monthly)
}