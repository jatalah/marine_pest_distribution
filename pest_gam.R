library(tidyverse)
library(mgcv)
library(gratia)
library(broom)
library(caret)

source('theme_javier.R')
theme_set(theme_javier())

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
pest_gam <-
  pest_long_nest %>%
  mutate(
    beta_gam = purrr::map(
      .x = data,
      ~ gam(
        cover_b ~
          s(latitude, longitude, k = 20) + 
          s(latitude) +
          s(longitude) +
          season * habitat,
          family = betar,
        data = .x
      )
    ),
    gam_table = purrr::map(.x = beta_gam, ~ tidy(.x, parametric = T, conf.int = T)),
    gam_table_s = purrr::map(.x = beta_gam, ~ tidy(.x)))


pest_gam_pp <-
  pest_gam %>%
  select(beta_gam) %>%
  mutate(pp_plots = purrr::map2(
    .x = beta_gam,
    .y = taxa,
    ~ plotmo(
      .x,
      trace = -1,
      ylim = NA,
      type = 'response',
      caption = .y,
      ngrid2 = 20,
      # persp.col = heat.colors(500),
      mfrow = c(2, 3)
    )
  ))
  
# extract GAM table-----

gam_tables <- 
pest_gam %>%
  dplyr::select(gam_table) %>%
  unnest(cols = c(gam_table)) %>%
  bind_rows(pest_gam %>%
              dplyr::select(gam_table_s) %>%
              unnest(cols = c(gam_table_s))) %>% 
  arrange(taxa) %>%
  filter(term != '(Intercept)') %>%
  mutate(
    term = fct_recode(
      term,
      Season =  "seasonSep 18",
      Habitat = "habitatNatural",
      `Season x Habitat` = "seasonSep 18:habitatNatural"
    )
  ) %>%
  mutate_if(is.numeric, ~round(., 2)) %>% 
  mutate(p.value = as.character(ifelse(p.value == 0, "<0.001", p.value))) %>% 
  print() %>% 
  write_csv('tables/beta_gam_tables.csv', na = "")


## GAMs for natural sites only -------
pest_nat <- 
  pest_long %>% 
  dplyr::filter(habitat=='Natural'& taxa !="Ciona robusta") %>% 
  ungroup()

pest_nat %>%
  gather(key, var, c(8:15)) %>%
  ggplot() +
  geom_histogram(aes(var)) +
  facet_wrap( ~ key, scales = 'free')
  

# VIF-----  
diag(solve(cor(dplyr::select(pest_nat,
                      carpophyllum,
                        cystophora,
                        bedrock,
                        boulder,
                        -cobble,
                      secchi,
                      slope,
                        distance_to_farm)))) %>% enframe()


##predictor transformation
preds <- 
  pest_nat %>%
  dplyr::select(carpophyllum,
                cystophora,
                bedrock,
                boulder,
                secchi,
                slope) %>% 
  data.frame() 

t_preds <- predict(preProcess(preds, method = c("BoxCox", "center", "scale")), preds)

t_preds %>%
  gather(key, var) %>%
  ggplot() +
  geom_histogram(aes(var)) +
  facet_wrap( ~ key, scales = 'free')

# nest data and add transformed predictors----
pest_nat_t <- 
pest_nat %>% 
  select(c(-names(preds))) %>%
  bind_cols(t_preds) %>%
  mutate(habitat = factor(habitat),
         season = factor(season)) %>% 
  group_by(taxa) %>% 
  nest()

library(betareg)

pest_gam_natural <-
  pest_nat_t %>%
  mutate(
    beta_reg = purrr::map(.x = data,
                          ~ stepAIC(glmmTMB(
                            cover_b ~ 
                              # distance_to_farm +
                              boulder +
                              bedrock +
                              cystophora  +
                              secchi +
                              slope +
                              carpophyllum,
                            family = beta_family(),
                          data = .x
                          ))),
    gam_table = purrr::map(.x = beta_reg, ~ tidy(.x,  conf.int = T)))


pest_gam_natural %>% mutate(gam_table_sel = map(.x = beta_reg, ~ stepAIC(.x)))

stepAIC(pest_gam_natural[[3]][1])

gam_tables_nat <-
  pest_gam_natural %>%
  dplyr::select(gam_table) %>%
  unnest(cols = c(gam_table)) %>%
  mutate_if(is.numeric, ~round(., 3)) %>%
  ungroup() %>% 
  dplyr::filter(term != '(Intercept)' & term != '(phi)') %>%
  dplyr::select(-component) %>% 
  mutate(p.value = as.character(ifelse(p.value == 0, "<0.001", p.value)),
         p.value = as.character(ifelse(p.value>0&p.value<0.01, "<0.01", p.value)),
         p.value = as.character(ifelse(p.value>=0.01&p.value<0.05, "<0.05", p.value))) %>%
  print() 

view(gam_tables_nat)

coef_plot <- 
  ggplot(gam_tables_nat,
       aes(x = estimate,
           y = fct_rev(term),
           color = term)) +
  geom_point() +
  geom_errorbarh(aes(
    xmin = conf.low ,
    xmax = conf.high,
    height = .2
  )) +
  facet_wrap(~ taxa) +
  geom_vline(xintercept = 0,
             lty = 2,
             col = 'gray60') +
  ylab('') +
  scale_x_continuous(breaks = seq(-.5,.5,.25)) +
  xlab('Coefficients') +
  theme(strip.text = element_text(face = "italic"),
        legend.position = 'none')

coef_plot

ggsave(
  coef_plot,
  file = 'figures/coef_plot.tiff',
  device = 'tiff',
  width = 8,
  height = 6,
  compression = 'lzw'
)


# x <- glmmTMB(cover_b ~ 
#                distance_to_farm +
#                boulder +
#                bedrock +
#                cystophora  +
#                secchi +
#                slope +
#                carpophyllum,
#              family = beta_family(),
#              data = pest_nat)
