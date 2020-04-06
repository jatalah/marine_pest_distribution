library(rgdal)
library(tidyverse)
library(mapdata)
library(sf)
library(ggspatial)
library(tmaptools)
library(mapview)

# read data--------
# marine farms--------------
farms <-
  st_read("C:/Users/javiera/OneDrive - Cawthron/R folder/Marine_Farms/Marine_Farms.shp") %>%
  st_transform(4326) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(polygons)) %>%
  set_projection(farms, projection = "longlat", current.projection = "longlat") %>%
  filter( PermitStat == 'Active' & DecisionSt=='Granted') 

nz_coast <-
  st_read('data/nz_coast/coast_wgs84.shp') %>%
  set_projection(farms, projection = "longlat", current.projection = "longlat")


pelorus_farms <- 
  st_crop(farms, c(ymax=-40.85, ymin = -41.3, xmin=173.76, xmax=174.132442)) %>% 
  st_transform(4326) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(polygons)) %>%
  set_projection(farms, projection = "longlat", current.projection = "longlat")

# cast points associated with each ID as a polygon
pelorus_farms1 <- 
  pelorus_farms %>% 
  group_by(MarineFarm) %>%
  summarise(geometry = st_union(geometry))


# calculate farm area-----
sum(st_area(pelorus_farms1))
dim(pelorus_farms1)
mapview(pelorus_farms1)

# plot of NZ coast-----------
nz_map <-
  ggplot(nz_coast) +
  geom_sf(fill = "gray90") +
  coord_sf(
    xlim = c(166, 178.8),
    ylim = c(-34.35, -47.35),
    expand = T
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = 'transparent'),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "transparent")
  ) +
  geom_point(aes(x = 173.90, y = -41.1), color = "#00BFC4", size = 4)
nz_map

pelorus_farms_map <-
  ggplot() +
  geom_sf(data = nz_coast, fill = "gray90") +
  geom_sf(data = pelorus_farms1, color = '#00BFC4', fill = 'transparent') +
  geom_sf(data = pest_sf, color = '#F8766D') +
  theme_bw() +
  coord_sf(xlim = c(173.75, 174.15),
           ylim = c(-40.85,-41.3)) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  geom_point(aes(y = -41.276498894, x =  173.7666636),
             color = "#7CAE00",
             size = 4) +
  annotate(
    x = 173.78,
    y = -41.285,
    geom = 'text',
    label = 'Havelock',
    size = 7
  ) +
  theme(
    # panel.grid.major = element_line(
    #   color = gray(0.5),
    #   linetype = "dashed",
    #   size = 0.5
    # ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "transparent")
  )

figure_1 <- 
  pelorus_farms_map +
  annotation_custom(
    grob = ggplotGrob(nz_map),
    xmin = 174.05,
    xmax = 174.17,
    ymin = -41.215,
    ymax = -41.315
  ) 

print(figure_1)

# save 
ggsave(filename = "figures/figure_1.tiff", 
       plot = figure_1, 
       device = 'tiff',
       width = 160,
       height = 208,
       units = "mm",
       dpi = 600,
       compression = 'lzw')


# using cowplot------
library(cowplot)
figura1 <- 
  ggdraw() +
  draw_plot(pelorus_farms_map) +
  draw_plot(nz_map, x = 0.55, y = 0.05, width = .25, height = .25)

ggsave(filename = "figure_1.tiff", 
       plot = figura1, 
       width = 20,
       height = 30,
       units = "cm",
       dpi = 300,
       compression = 'lzw')
