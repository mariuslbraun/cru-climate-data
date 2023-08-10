# load packages
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(rvest)

# clear workspace
rm(list = ls())



# download country climate data ----

# function to retrieve all URLs from website that match pattern
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

# set climate variable for which data should be retrieved
climate_var = "tmp"
dir.create(file.path("raw", climate_var), showWarnings = F)

# get URLs to country files
link = paste0(
  "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/crucy.2304181636.v4.07/countries/",
  climate_var
)
country_files = get_links(link, ".per")
country_files$country = str_remove(
  gsub(".*2022.","",country_files$url),
  ".tmp.per"
)

# download country files
for(i in 1:length(country_files$url)) {
  file_path = file.path("raw", climate_var, country_files$url[i])
  if(!file.exists(file_path)) {
    download.file(
      url = file.path(link, country_files$url[i]),
      destfile = file_path
    )
  }
}
rm(i)

# clean up country climate data ----
tidy_climate_var = function(i) {
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
  country_data_monthly$year = as.factor(country_data_monthly$year)
  country_data_monthly$month = as.factor(country_data_monthly$month)
  country_data_monthly$tmp = as.numeric(country_data_monthly$tmp)
  country_data_monthly$tmp = ifelse(
    country_data_monthly$tmp == -999.0,
    NA,
    country_data_monthly$tmp
  )
  country_data_monthly$country = as.factor(country_data_monthly$country)
  
  return(country_data_monthly)
}

# iterate over country list ----

# create data frame name
df_name = paste(
  climate_var,
  "data",
  sep = "_"
)

# rbind country data frames and assign to data frame name
assign(
  x = df_name,
  value = do.call(
    rbind.data.frame, lapply(
    X = 1:length(country_files$url),
    FUN = tidy_climate_var
    )
  )
)

saveRDS(
  get(df_name),
  file.path("prepared", paste0(df_name, ".rds"))
)