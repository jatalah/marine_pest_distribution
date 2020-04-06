library(sf)
library(mapview)
library(tidyverse)
library(tmaptools)

# pest data --------
pest <- read_csv('data/cleaned_data/pest_data.csv')

pest_sf <- 
  pest %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

# marine farms--------------
farms <-
  st_read("C:/Users/javiera/OneDrive - Cawthron/R folder/Marine_Farms/Marine_Farms.shp") %>%
  st_transform(4326) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(polygons)) %>%
  set_projection(farms, projection = "longlat", current.projection = "longlat") %>%
  filter(MarineFa_3 == 'Active') 

# nz coast---------
nz_coast <- 
  st_read('data/nz_coast/coast_wgs84.shp') %>% 
   set_projection(farms,projection = "longlat",current.projection = "longlat")


st_area(farms)

# merge dataset------
#farms_pest <- st_join(active_farms, pest_sf)

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

pest <- bind_cols(distance_2_farm, pest) 


#   write_csv('data/cleaned_data/pest_dist.csv')


# visualise data -------
mapview(active_farms) +
  mapview(pest_sf, cex = "Cladophora ruchengeri", zcol = 'Cladophora ruchengeri', alpha = 0.3)


# 2. Pelorus reefs shapefile--------
reefs <- 
  st_read("C:/Users/javiera/OneDrive - Cawthron/R folder/marlborough reefs/reefs.shp") %>% 
  st_transform(crs = 4326) 

reefs_cook <- 
  reefs %>% 
  filter(BIO_TYP =="South Cook Strait") %>% 
  mutate(Area = as.numeric(st_area(.)))

mapview(reefs_cook['NZ5_BIOREG'])


# create uniform grid for reefs area-------------
reefs_grid <- 
  reefs_cook %>% 
  st_make_grid(cellsize = .005, square = FALSE)

plot(reefs_grid)


nearest_farms_2reef <- 
  active_farms %>%  # find the nearest farm for each sampling site##
  slice(st_nearest_feature(x = reefs_grid, y = active_farms))


distance_2reef <- 
  st_distance(x = reefs_grid, y= nearest_farms_2reef, by_element = TRUE) %>% 
  as.numeric() %>% 
  tibble() %>%
  rename(distance_to_farm = '.')

#  get centroid coordinates-----
reef_coords <- 
  st_centroid(reefs_grid) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  rename(longitude = X, latitude = Y)

reefs_data <- 
  bind_cols(reef_coords, distance_2reef) %>% 
  mutate(Habitat = "Natural")

####################################
reefs_data$pred <- predict(gam_nat,newdata = reefs_data,type = 'response')

reefs_data_sf <- 
  reefs_data %>% 
  select(-latitude, - longitude) %>% 
  bind_cols(., as.data.frame(reefs_grid)) %>% 
  st_sf(sf_column_name = 'geometry')

ggplot() +
  geom_sf(data = nz_coast, fill = 'gray95') +
  # geom_sf(data = reefs_data_sf, fill =  'darkgreen') +reefs_data_sf
  geom_sf(data = reefs_data_sf,
          aes(fill = pred * 100),
          alpha = 0.8) +
  coord_sf(xlim = c(173.75, 174.2),
           ylim = c(-40.9, -41.3)) +
  theme_bw() +
  scale_fill_gradientn(colours = rev(heat.colors(100)))



# maps ----
ggplot() +
  geom_sf(data = nz_coast, fill = 'gray95') +
  geom_sf(data = active_farms, fill = 'darkred') +
  geom_sf(data = reefs_cook, fill =  'darkgreen') +
  geom_sf(data = pest_sf, aes(color = `Cladophora ruchengeri`), size = 2) +
  scale_color_gradientn(colours = rev(heat.colors(100))) +
  coord_sf(xlim = c(173.75, 174.2),
           ylim = c(-40.9, -41.3)) +
  theme_void()

ggplot() +
  geom_sf(data = nz_coast, fill = 'gray95') +
  geom_sf(data = active_farms, color = 'darkred') +
  geom_sf(data = pest_sf, color = 'darkgreen') +
  coord_sf(xlim = c(173.75, 174.2),
           ylim = c(-40.9, -41.25)) +
  theme_javier()

mapview(active_farms) +
  mapview(active_farms) +
  mapview(pest_sf, zcol = "Cladophora ruchengeri", alpha = 0.3)