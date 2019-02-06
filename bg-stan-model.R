library(tidyverse)
library(ggthemes)
library(brms)

train <- readRDS('./data/simulated_landspeeder_data_train.rds')
test <- readRDS('./data/simulated_landspeeder_data_test.rds')

mod_04_multilevel_planet <-
    brm(total_claims ~ (1 | planet) + pilot + vehicle +
            (0 + pilot + vehicle | planet),
        data = train, family = poisson(link = 'log'),
        prior = prior(normal(0, 5)),
        control = list(adapt_delta = 0.85),
        cores = 2, chains = 2, iter = 2000)

saveRDS(mod_04_multilevel_planet, './data/mod_04_multilevel_planet_bg.rds')
