library(tidyverse)
library(lme4)

train <- readRDS('./data/simulated_landspeeder_data_train.rds')
test <- readRDS('./data/simulated_landspeeder_data_test.rds')

mod_04_multilevel_planet <-
    glmer(total_claims ~ (1 | planet) + pilot + vehicle +
              (0 + pilot + vehicle | planet),
          data = train, family = poisson(link = 'log'))

saveRDS(mod_04_multilevel_planet, './data/mod_04_lme4_planet_bg.rds')
