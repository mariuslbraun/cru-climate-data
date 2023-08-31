# load packages
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(rvest)
library(sf)
library(magick)

# clear workspace
rm(list = ls())

# source functions
source("scripts/functions.R")



# load shapefile ----
country_shape = read_sf("raw/shapefile/WB_countries_Admin0_10m.shp") %>%
  select(NAME_EN, geometry, ISO_A3_EH)
country_shape$NAME_EN = str_replace_all(
  country_shape$NAME_EN, " ", "_"
)
country_shape = country_shape[!duplicated(country_shape$NAME_EN), ]

# rename countries in shapefile
lookup_country_names = read_csv("interim/lookup_country_names.csv", col_names = T)
for(i in 1:nrow(lookup_country_names)) {
  country_shape$NAME_EN = str_replace_all(
    country_shape$NAME_EN,
    lookup_country_names$old_name[i],
    lookup_country_names$replace_name[i]
    )
}
rm(i, lookup_country_names)



# create maps of monthly climatic anomalies for 1901-2021 ----
climate_var = "tmp"
years = 1901:2021
months = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

file_path = file.path("maps", climate_var)
dir.create(file_path, showWarnings = F, recursive = T)
for(i in 1:length(years)) {
  for(j in 1:length(months)) {
    df = readRDS(
      file.path(
        "prepared",
        paste0(climate_var, "_data.rds")
      )
    ) %>% filter(
      year == years[i],
      month == months[j]
    )
    
    # join shapefile with climate data
    country_shape_new = left_join(
      country_shape,
      df,
      by = join_by(NAME_EN == country),
      keep = FALSE
    )
    
    # create map of climatic anomalies
    ggplot(data = country_shape_new) +
      geom_sf(aes(fill = anom_1901_2000)) +
      scale_fill_viridis_c(option = "plasma", trans = "reverse") +
      guides(fill=guide_legend(title="")) +
      labs(
        title = paste(climate_var, "anomaly relative to 1901-2000", sep = " "),
        subtitle = paste(years[i], months[j], sep = " ")
      )
    theme_bw() +
      theme(panel.grid.major = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())
    anom_name = paste(
      climate_var, "anom", years[i], months[j], sep = "_"
    )
    ggsave(
      file.path(
        file_path,
        paste0(anom_name, ".png")
      )
    )
  }
}
rm(i, j, anom_name)



# create animated GIF of climatic anomalies map ----
# clear memory
gc()

## list file names and read in
imgs = list.files(file_path, full.names = TRUE)
img_list = lapply(imgs, image_read)

## join the images together
img_joined = image_join(img_list)

## animate at 2 frames per second
img_animated = image_animate(img_joined, fps = 5)

## view animated image
img_animated

## save to disk
image_write(
  image = img_animated,
  path = file.path(
    file_path,
    paste0(
      climate_var,
      "_anom_1901_2000_animated",
      ".gif"
    )
  )
)
