# load packages
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(rvest)
library(sf)
library(transformr)
library(png)
library(gifski)
library(gganimate)

# clear workspace
rm(list = ls())

# source functions
source("scripts/functions.R")



# load shapefile ----
country_shape = read_sf("raw/shapefile/WB_countries_Admin0_10m.shp") %>%
  select(NAME_EN, geometry)
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
start_year = 2000
end_year = 2021

climate_var_name = as.character(
  (
    read.csv("interim/lookup_varname.csv") %>%
      filter(X...var_name == "tmp")
    )$var_text
)
  

# load climate data frame
df = readRDS(
  file.path(
    "prepared",
    paste0(climate_var, "_data.rds")
  )
) %>% filter(year >= start_year &
             year <= end_year)

# join climate data frame with geometry from shape file
df = right_join(
  df,
  country_shape,
  by = join_by(country == NAME_EN),
  keep = TRUE
)

# add year-month column
df = df %>%
  mutate(month = recode(month,
                        "JAN" = "01",
                        "FEB" = "02",
                        "MAR" = "03",
                        "APR" = "04",
                        "MAY" = "05",
                        "JUN" = "06",
                        "JUL" = "07",
                        "AUG" = "08",
                        "SEP" = "09",
                        "OCT" = "10",
                        "NOV" = "11",
                        "DEC" = "12"
    )
  )
df$year_month = paste(
  df$year,
  df$month,
  "01",
  sep = "-"
)
df$year_month = as.Date(df$year_month)

# convert data frame to sf object
df = st_as_sf(df)
df = df %>% select(-c(tmp, NAME_EN))

# create map of climatic anomalies
map_animated = ggplot(data = df) +
  geom_sf(aes(fill = anom_1901_2000, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma", trans = "reverse") +
  guides(fill=guide_legend(title="")) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  transition_manual(frames = year_month) +
  labs(
    title = paste("Monthly", climate_var_name, "anomaly relative to 1901-2000", sep = " "),
    subtitle = "{format(as.Date(current_frame), '%Y-%m')}"
  ) +
  coord_sf(default_crs = st_crs(df), datum = NA)
animate(map_animated, fps = 2)
anim_save(
  file.path(
    "maps",
    paste0(climate_var, "_anom_map_animated.gif")
  )
)