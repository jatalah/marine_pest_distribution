library(tidyverse)
library(broom)


# read data----
pest_long <- 
  read_csv('data/cleaned_data/pest_data_long.csv')

# nest data ----
pest_long_nest <-
  pest_long %>%
  mutate(habitat = factor(habitat),
         season = factor(season)) %>% 
  group_by(taxa) %>% 
  nest()


# fit a beta regression model---- 
pest_nls <- 
  pest_long_nest %>%
  mutate(nls_models = purrr::map(
    .x = data,
    ~nls(cover_b*100 ~ a * exp(b * distance_to_farm) ,
         start = list(a = 50, b = -.01),
         control = list(maxiter = 500),
         trace = TRUE,
      data = .x
    )
  ),
  nls_summary = map(.x = nls_models, tidy),
  nls_predict = map(.x = nls_models, augment),
  nls_glance = map(.x = nls_models, glance))


# summary tables-----
nls_summary_table <- 
  pest_nls %>% 
  dplyr::select(taxa, nls_summary) %>% 
  unnest(cols = nls_summary) %>%
  ungroup() %>% 
  mutate_if(is.numeric, ~round(., 3)) %>% 
  mutate(p.value = as.character(ifelse(p.value == 0, "<0.001", p.value)),
         p.value = as.character(ifelse(p.value>0&p.value<0.01, "<0.01", p.value)),
         p.value = as.character(ifelse(p.value>=0.01&p.value<0.05, "<0.05", p.value))) %>% 
  print() %>% 
  write_csv('tables/exponential_decay_models.csv')


# R2 for the decay models
nls_augment %>% 
  group_by(taxa) %>% 
  summarise(RRS= sum(.resid^2),
            TSS =sum((cover_b - mean(cover_b))^2),
            R2 = 1 - (RRS/TSS))

# glance outputs---see ?glance.nls
pest_nls %>% 
  dplyr::select(taxa, nls_glance) %>% 
  unnest(cols = nls_glance) %>%
  ungroup()

# plot results ----
nls_augment <- 
  pest_nls %>% 
  dplyr::select(nls_predict) %>% 
  unnest(cols = c(nls_predict)) 


# predict new data ------
new_dat <- tibble(distance_to_farm = 1:5000)
predict(nls_blues,newdata = new_dat) %>% 
  enframe() %>% 
  bind_cols(new_dat)

x <- 
  pest_nls %>% 
  mutate(pred = map(nls_models, ~predict(.x, newdata = new_dat))) %>% 
  select(pred) %>%
  unnest(c(pred)) %>% 
  ungroup() %>% 
  bind_cols(d = rep(1:5000, 10))

ggplot(x,aes(d, pred)) +
  geom_line() +
  facet_wrap(~taxa)


x %>% 
  group_by(taxa) %>% 
  nest() %>% 
  mutate(z = map(data, ~ .$d[which.max(.$pred<0.01)])) %>%
  unnest(cols = z) %>% 
  arrange(z)


  ggplot(nls_augment, aes(x = distance_to_farm/1000)) + 
  geom_point(aes(y = cover_b), alpha = .4, size = 3, position = position_jitter()) +
  geom_line(aes(y = .fitted), color = 2, size = 1) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  scale_x_sqrt(breaks = c(0, 0.1, 0.5, 1, 2, 3)) +
  theme_javier() +
  labs(x = 'Distance to nearest farm (km)', y = "Cover (%)") +
  theme(strip.text = element_text(face = "italic"),
        legend.position = 'none')
  

# Using ggplot------------
exponential_curves_plot <- 
  pest_long %>%
  ggplot(aes(x = distance_to_farm / 1000, y = cover)) +
  geom_point(alpha = .3,
             size = 3,
             position = position_jitter(width = .1),
             aes(color = habitat)) +
  geom_smooth(
    method = "nls",
    formula = y ~ a * exp(b * x),
    method.args = list(start = c(a = 10, b = -.1)),
    se = FALSE,
    color = 'darkred'
  ) +
  facet_wrap(~ taxa, scales = 'free', nrow = 2) +
  scale_x_sqrt(breaks = c(0, 0.25, 1, 2, 3, 4, 5), labels = c(0, 0.25, 1, 2, 3, 4, 5 )) +
  theme_javier() +
  labs(x = 'Distance to nearest farm (km)', y = "Cover (%)") +
  theme(strip.text = element_text(face = "italic"),
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



## indiviudla taxa---
nls_blues <- 
  nls(
  `Mytilus galloprovincialis` ~ a * exp(b * distance_to_farm) ,
  start = list(a = 50, b = -.01),
  data = pest
)


clado_nls <-
  nls(
    `Cladophora ruchengeri` ~ a * exp(b * distance_to_farm) ,
    start = list(a = 10, b = -.01),
    data = pest
  )
summary(clado_nls)




  
