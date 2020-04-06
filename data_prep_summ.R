library(tidyverse)
library(readxl)
library(sf)

source('theme_javier.R')
theme_set(theme_javier())

# load data-----------
pest <-
  read_csv('data/Copy of cawthron_target_pests_summer_2018.csv') %>%
  mutate(
    date = as.Date(created_at),
    season = if_else(date > '2018-4-01', 'Sep 18', 'Feb 18'),
    site_type = fct_recode(
      site_type,
      Farm = "mussel_farm",
      Adjacent = "natural_adjacent",
      Distant = "natural_distant"
    ),
    site_type = fct_relevel(site_type, "Farm", "Adjacent"),
    habitat= fct_collapse(site_type, Natural = c("Distant", "Adjacent")),
    slope = as.numeric(fct_recode(slope, "2" = "Moderate 30-60","1" = "Gentle 0-30", "3" = "Steep 60-90"))
  ) %>% 
  dplyr::select(
    date,
    season,
    latitude,
    longitude,
    site_no,
    site_type,
    habitat,
    carpophyllum,
    cystophora,
    bedrock,
    boulder,
    cobble,
    secchi,
    slope,
    ciona:styela
  ) %>%
  full_join(read_csv('data/cleaned_data/distance_to_farm.csv'), by = c('latitude', "longitude")) %>%   # from Paula's calculations
  rename(
    `Ciona robusta` = "ciona",
    `Cladophora ruchengeri` = "cladophora",
    `Didemnum vexillum` = "dv",
    `Styela clava` = "styela",
    `Mytilus galloprovincialis` = "mytilus",
    `Codium fragile` = "codium",
    `Undaria pinnatifida` = "undaria",
    `Pylaiella littoralis` = "ectocarpus",
    Ceramiales = "filbrown",
    `Colpomenia spp.` =  "colpomenia"
  ) %>%
  mutate_if(is.numeric,  ~ if_else(is.na(.), 0, .)) %>% 
  dplyr::select(date:slope, distance_to_farm, everything()) %>% 
  write_csv('data/cleaned_data/pest_data.csv')


# long format data
pest_long <- 
  pest %>% 
  gather(taxa, cover, `Ciona robusta`: `Styela clava` ) %>%   
  group_by(taxa) %>% 
  mutate(cover_b = ((cover/100) * (n()-1) + 0.5) /  n(),
         site_type = factor(site_type),
         season = factor(season)) %>% 
  write_csv('data/cleaned_data/pest_data_long.csv')


# calculate distance froim farms----------
pest_sf <- 
  pest %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

# marine farms--------------
farms <-
  st_read("C:/Users/javiera/OneDrive - Cawthron/R folder/Marine_Farms/Marine_Farms.shp") %>%
  st_transform(4326) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(polygons)) %>%
  set_projection(farms, projection = "longlat", current.projection = "longlat")

active_farms <- 
  farms %>% 
  filter(MarineFa_3=='Active') 


# distance to farm calculation ----
nearest_farms_2site <- 
  active_farms %>%  # find the nearest farm for each sampling site##
  slice(st_nearest_feature(x = pest_sf, y = active_farms))

# calculate distance to closest farm--
distance_2_farm <- 
  st_distance(x = pest_sf, y= nearest_farms_2site, by_element = TRUE) %>% 
  as.numeric() %>% 
  tibble() %>%
  rename(distance_to_farm = '.')


