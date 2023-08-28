# load packages
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(rvest)
library(sf)

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

# load data for given variable for given year and month
climate_var = "tmp"
year_ = 1901
month_ = "JUL"

df = readRDS(
  file.path(
    "prepared",
    paste0(climate_var, "_data.rds")
  )
) %>% filter(year == year_, month == month_)

# join shapefile with climate data
country_shape = left_join(
  country_shape,
  df,
  by = join_by(NAME_EN == country),
  keep = FALSE
)

# create map of climatic anomalies
ggplot(data = country_shape) +
  geom_sf(aes(fill = anom_1901_2000)) +
  scale_fill_viridis_c(option = "plasma", trans = "reverse") +
  guides(fill=guide_legend(title="")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
#ggsave("figures/maps/disasters_total.pdf")
