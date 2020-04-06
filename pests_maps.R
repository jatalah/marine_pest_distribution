library(readxl)
library(rgdal)
library(tidyverse)
library(ggmaps)
library(mapdata)
library(sf)
library(viridis)
source('theme_javier.R')

theme_set(theme_javier())

# read data

pest_long <- read_csv('data/cleaned_data/pest_data_long.csv')

pest_long_mean <- 
  pest_long %>% 
  group_by(taxa, latitude, longitude) %>% 
  summarise(cover = mean(cover, na.rm =T))
# Plot map with farms
nz <- map_data('nzHires')

p <- 
  ggplot() +
  geom_polygon(
    data = nz,
    aes(x = long, y = lat, group = group),
    colour = "grey50",
    fill = 'grey95'
  ) + coord_map(xlim = c(173.75, 174.31),
               ylim = c(-40.7, -41.3))
  # geom_path(
  #   data = farms_df,
  #   aes(x = long, y = lat, group = group),
  #   color = 'red',
  #   size = .2
  # ) +
  # labs(y = 'Longitude',  x = 'Latitude') +
  # theme_bw() 
  
## All maps
all_maps <-
  p +
  geom_point(data = pest_long,
             aes(
               x =  longitude,
               y = latitude,
               size = cover,
               color = taxa
             )) +
  scale_size() +
  facet_grid(season ~ taxa) +
  scale_color_viridis_d(guide = F) +
  theme(legend.position = 'bottom')

print(all_maps)

ggsave(
  all_maps,
  file = 'figures/pest_maps.tif',
  device = 'tiff',
  width = 20,
  height = 8,
  compression = 'lzw'
)

# faceted maps of pests--- 
maps_pest <- 
  ggplot() +
  geom_polygon(
    data = nz,
    aes(x = long, y = lat, group = group),
    colour = "gray50",
    fill = 'grey97'
  ) + coord_map(xlim = c(173.72, 174.25),
                ylim = c(-40.7, -41.3)) +
  geom_point(
    data = pest_long,
    aes(
      x =  longitude,
      y = latitude,
      size = ifelse(cover==0,NA,cover),
      fill = season
    ),
    position = position_jitter(width = .02),
    # color = 1,
    shape = 21,
    alpha = 0.45
  ) +
  scale_size(name = 'Cover', range = c(2,6)) +
  theme_void() +
  facet_wrap(~taxa,nrow = 2) +
  scale_fill_discrete(name = "Season") +
  theme(legend.position = 'bottom',
        strip.text = element_text(face = "italic"))


ggsave(
  maps_pest,
  file = 'figures/pest_maps4.tif',
  device = 'tiff',
  width = 7.3*1.4,
  height = 4.6*1.4,
  compression = 'lzw'
)
 #########################

## Site type boxplot ###
site_type_boxplot <-
  ggplot(pest_long, aes(x = site_type, y = cover +1)) +
  geom_boxplot(aes(fill = site_type)) +
  facet_grid(season~key) +
  scale_y_log10() +
  labs ( y = '% cover ', x = 'Site')

  

# mean sd plots------
ggplot(pest_long, aes(x = site_type, y = cover + 1, color = season)) +
  stat_summary(
    fun.data = mean_se,
    size = .5,
    position = position_dodge(width = .5),
    alpha = 0.5
  ) +
  facet_wrap( ~ taxa, scales = 'free_y') +
  scale_y_log10() +
  labs (y = '% cover ', x = '') +
  # theme_javier(base_size = 16)
  # coord_flip() +
  theme(legend.position = 'bottom')


  
ggsave(site_type_boxplot,
       file = 'figures/site_type_boxplot.tif',
       device = 'tiff',
       width = 12,
       height = 6,
       compression = 'lzw')


##distance----------------
distance2farm_pest_plot <- 
  pest_long %>%
  # filter(cover > 0) %>%
  ggplot(., aes(linear_dist2farm, cover)) +
  geom_point(aes(color = site_type), alpha = .7) +
  facet_wrap( ~ key, scales = 'free') +
  labs(y = '% cover ', x= 'Linear distance to farm (m)') +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(se = F, span = 1)

ggsave(distance2farm_pest_plot,
       file = 'figures/distance2farm_pest_plot.tif',
       device = 'tiff',
       width = 12,
       height = 6,
       compression = 'lzw')

