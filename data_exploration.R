library(tidyverse)
library(splines)
library(quantreg)

pest_long <- read_csv('data/cleaned_data/pest_data_long.csv')

# data summaries----
pest_long %>%
  group_by(taxa) %>%
  summarise_at(
    "cover",
    list(
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      # Q1 = ~quantile(., probs = 0.25,na.rm = T),
      # Q3 = ~quantile(., probs = 0.75,na.rm = T),
      se = ~ sd / sqrt(n()),
      ci = ~ se * 1.96
    ),
    na.rm = T
  ) %>%
  mutate(mean = round(mean,2),
         se = round(se,3),
         sd = round(sd,3)) %>% 
  arrange(desc(mean)) %>% 
  print() %>% 
  write_csv('tables/pest_cover_summary.csv')


# summarise by habitat type --------------
pest_by_habitat_sum <-
  pest_long %>%
  group_by(taxa, habitat) %>%
  summarise_at(
    "cover",
    list(
      prop_p = ~ sum(. > 0) / length(.) * 100,
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      se = ~ sd / sqrt(n()),
      ci = ~ se * 1.96
    )
  ) %>% 
  print()

# By season -----
pest_long %>%
  group_by(taxa, season) %>%
  summarise_at(
    "cover",
    list(
      prop_p = ~ sum(. > 0) / length(.) * 100,
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      se = ~ sd / sqrt(n()),
      ci = ~ se * 1.96
    )
  ) %>% 
  print()



# Explore distance and habitat ----
ggplot(pest) +
  geom_histogram(aes(distance_to_farm)) +
  facet_wrap(~site_type)

pest %>% 
  group_by(site_type) %>% 
  summarise_at(
    "distance_to_farm",
    list(
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      se = ~ sd / sqrt(n()),
      ci = ~ se * 1.96
    ))


# Site type boxplot --------------
pest_boxplot <- 
  ggplot(pest_long, aes(x = site_type, y = cover +1)) +
  geom_boxplot() + 
  facet_wrap(~taxa, scales = 'free') +
  labs ( y = '% cover ', x = 'Site') +
  scale_y_log10()


# distance to farm-----------
ggplot(pest_long,
       aes(x = distance_to_farm, y = cover)) +
  geom_point(alpha = 0.5, size = 3) +
  facet_wrap( ~ taxa, scales = 'free', nrow = 2) +
  geom_smooth(method = 'loess', se = F, span = .9) +
  # scale_y_log10() +
  labs (y = '% cover ', x = "Distance to farm (m)") +
  theme_javier()

pest_long %>%
  filter(habitat == "Natural") %>%
  ggplot(aes(x = distance_to_farm, y = cover)) +
  geom_point(alpha = 0.5, size = 3) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  geom_smooth(method = 'gam', se = F, span = .9) +
  # scale_y_log10() +
  labs (y = '% cover ', x = "Distance to farm (m)") +
  theme_javier()

# carpophyllum----
pest_long %>%
  filter(habitat == "Natural") %>%
  ggplot(aes(x = carpophyllum, y = cover)) +
  geom_point(alpha = 0.5, size = 3) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  geom_smooth(method = 'loess', se = F, span = .9) +
  # scale_y_log10() +
  labs (y = '% cover ') +
  theme_javier()

# cystophora---
pest_long %>%
  filter(habitat == "Natural") %>%
  ggplot(aes(x = cystophora, y = cover)) +
  geom_point(alpha = 0.5, size = 3) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  geom_smooth(method = 'loess', se = F, span = .9) +
  # scale_y_log10() +
  labs (y = '% cover ') +
  theme_javier()

# bedrock---
pest_long %>%
  filter(habitat == "Natural") %>%
  ggplot(aes(x = bedrock, y = cover)) +
  geom_point(alpha = 0.5, size = 3) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  geom_smooth(method = 'loess', se = F, span = .9) +
  # scale_y_log10() +
  labs (y = '% cover ') +
  theme_javier()


# cobble---
pest_long %>%
  filter(habitat == "Natural") %>%
  ggplot(aes(x = cobble, y = cover)) +
  geom_point(alpha = 0.5, size = 3) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  geom_smooth(method = 'loess', se = F, span = .9) +
  # scale_y_log10() +
  labs (y = '% cover ') +
  theme_javier()


# boulder---
pest_long %>%
  filter(habitat == "Natural") %>%
  ggplot(aes(x = boulder, y = cover)) +
  geom_point(alpha = 0.5, size = 3) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  geom_smooth(method = 'loess', se = F, span = .9) +
  # scale_y_log10() +
  labs (y = '% cover ') +
  theme_javier()

# quantile regression distance to farm------------
pest_long %>%
  # filter(habitat == "Natural") %>%
  ggplot(aes(x = distance_to_farm/1000, y = cover)) +
  geom_point(size = 3, alpha = .3) +
  stat_quantile(method = rq,
                formula = y ~ bs(x, df = 3),
                quantiles = .95) +
  facet_wrap( ~ taxa, scales = 'free', nrow = 2) + 
  scale_x_sqrt(breaks = c(0, 0.1, 0.5, 1, 2, 3)) +
  labs (x = 'Distance (m) ', y = 'Cover (%)') +
  theme_javier() 


# Exponential decay curves---------------
exponential_curves_plot <- 
  pest_long %>%
  # filter(!(taxa=='Ciona robusta'&cover_b>.20)) %>% 
  ggplot(aes(x = distance / 1000, y = cover)) +
  geom_point(alpha = .3,
             size = 3) +
  geom_smooth(
    method = "nls",
    # formula = y ~ SSasymp(x, Asym, R0, lrc),
    formula = y ~ a * exp(-b * x),
    method.args = list(start = c(a = 70, b = .1)),
    se = FALSE,
    color = 'darkred'
  ) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  scale_x_sqrt(breaks = c(0, 0.1, 0.5, 1, 2, 3)) +
  theme_javier() +
  labs(x = 'Distance (km)', y = "Cover (%)") +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.position = 'none')
exponential_curves_plot

ggsave(
  exponential_curves_plot,
  file = 'figures/exponential_curves_plot.tiff',
  device = 'tiff',
  width = 11,
  height = 5,
  compression = 'lzw'
)


# natural only ------------
pest_long %>%
  # filter(habitat=='Natural') %>% 
  ggplot(aes(x = distance_to_farm / 1000, y = cover, color = season)) +
  geom_point(alpha = .3,
             size = 3) +
  geom_smooth(
    method = "nls",
    formula = y ~ a * exp(-b * x),
    method.args = list(start = c(a = 70, b = .1)),
    se = FALSE,
    color = 'darkred'
  ) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  scale_x_sqrt(breaks = c(0, 0.1, 0.5, 1, 2, 3)) +
  theme_javier() +
  labs(x = 'Distance (km)', y = "Cover (%)") +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.position = 'none')


# boxplot of site type by season--------
boxplot_season_type <- 
  pest_long %>% 
  mutate(site_type = fct_recode(site_type, 
                                `Farm` = "Farm",
                                Adj. = "Adjacent",
                                Dist. = "Distant"),
         site_type = fct_relevel(site_type, "Farm","Adj.")) %>% 
  ggplot(aes(x = habitat, y = cover + 1, color = season)) +
  geom_boxplot() +
  facet_wrap( ~ taxa, nrow = 2) +
  scale_y_log10() +
  theme_javier() + 
  scale_color_discrete(name = NULL) +
  labs(y = 'Cover (%)') +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.position = 'none')

boxplot_season_type

ggsave(
  boxplot_season_type,
  file = 'figures/boxplot_season_type.tif',
  device = 'tiff',
  width = 11,
  height = 5,
  compression = 'lzw'
)

# Bar plots habitat -----
bar_habitat_season <- 
  pest_long %>% 
  ggplot(aes(habitat, cover, fill = season)) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    position = position_dodge(width = .9),
    color = 1
  ) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(width = .9),
    geom = "errorbar",
    width = 0.2
  ) +
  labs(y = 'Cover (%)') +
  facet_wrap(~taxa, scales = 'free', nrow = 2) +
  theme_javier(base_size = 18)  + 
  theme(axis.title.x = element_blank(),
                        strip.text = element_text(face = "italic"),
                        legend.position = 'none') +
  scale_fill_grey()


# save plot-------
ggsave(
  bar_habitat_season,
  file = 'figures/bar_habitat_season.tiff',
  device = 'tiff',
  width = 14,
  height = 5,
  compression = 'lzw'
)


bar_season_type <- 
  pest_long %>% 
  mutate(site_type = fct_recode(site_type, 
                                `Farm` = "Farm",
                                Adj. = "Adjacent",
                                Dist. = "Distant"),
         site_type = fct_relevel(site_type, "Farm","Adj.")
  ) %>% 
  ggplot(aes(site_type, cover, fill = season)) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    position = position_dodge(width = .9),
    color = 1
  ) +
  stat_summary(
    fun.data = mean_se,
    position = position_dodge(width = .9),
    geom = "errorbar",
    width = 0.2
  ) +
  labs(y = 'Cover (%)') +
  facet_wrap(~taxa, scales = 'free', nrow = 2) +
  theme_javier(base_size = 18)  + 
  theme(axis.title.x = element_blank(),
        strip.text = element_text(face = "italic"),
        legend.position = 'none') +
  scale_fill_grey()


# season boxplot--------
ggplot(pest_long, aes(x = season, y = cover + 1 )) +
  geom_boxplot()  +
  facet_wrap(~taxa, scales = 'free') +
  scale_y_log10()


# Latitude vs pest by site type---
ggplot(pest_long, aes(x = latitude, y = cover, color = site_type)) +
  geom_point(alpha = .5, position = position_jitter(width = .05)) +
  geom_smooth(method = 'auto', se = F) +
  facet_wrap( ~ taxa, scales = 'free') +
  theme_javier()





