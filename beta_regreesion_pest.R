library(tidyverse)
library(glmmTMB)

source('theme_javier.R')
theme_set(theme_javier())

# load data-----------
pest_long<- read_csv('data/cleaned_data/pest_data_long.csv')

# beta regression models-----------------
library(betareg)
library(MASS)
library(glmmTMB)
library(broom.mixed)

# transfor blues data into proportion and beta transformation (exclude zeros) 
pest_beta_dat <-
  pest_beta %>%
  group_by(taxa) %>% 
  nest()

# fit a beta regression model---- 
pest_glm <- 
  pest_beta_dat %>%
  mutate(beta_glm = purrr::map(
    .x = data,
    ~glmmTMB(
      cover_b ~
        scale(latitude) * site_type + scale(longitude) + season + poly(linear_dist2farm,2),
      family = beta_family(),
      data = .x
    )
  ),
  glm_table = purrr::map(.x= beta_glm, ~tidy(.x)))

pest_glm_table <-
  pest_glm %>%
  dplyr::select(glm_table) %>%
  unnest(cols = c(glm_table)) %>%
  # filter(term != '(Intercept)') %>%
  dplyr::select(-component) %>%
  # mutate(
  #   term = fct_recode(
  #     term,
  #     `Site Distant` = "site_typeDistant",
  #     `Site farm` = "site_typeFarm",
  #     Latitude = "scale(latitude)",
  #     Longitude = "scale(longitude)",
  #     `Season Sept` =  "seasonSep 18",
  #     Distance = "scale(linear_dist2farm)",
  #     `Latitude x Distant` = "scale(latitude):site_typeDistant",
  #     `Latitude x Farm` = "scale(latitude):site_typeFarm"
  #   )
  # ) %>%
mutate_at(vars(estimate:statistic),  ~ round(., 2)) %>%
  dplyr::select(-effect) %>%
  write_csv('tables/beta_regression_tables.csv', na = "")

# fitted a mixed model with site as a random factor-------
# algae_glm_table %>% 
#   filter(term != "Site random effect") %>% 
#   ggplot(aes(x = term)) +
#   geom_point(aes(y = estimate)) +
#   geom_errorbarh(aes(y = estimate, xmin = estimate - std.error, xmax =estimate - std.error)) +
#   # geom_linerange() +
#   facet_wrap(~taxa) +
#   coord_flip() +
#   geom_hline(yintercept = 0, lty = 2)


