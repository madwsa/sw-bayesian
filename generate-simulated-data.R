library(tidyverse)
set.seed(354678)

# The two planets in our thought experiment
const_planets <- c('a.) Tatooine',
                   'b.) Hoth')

# 10k people from Tatooine, 1k people on Hoth in each of train, test
const_n <- c(20000, 2000)

planet <- unlist(mapply(rep,
                        x = const_planets,
                        times = const_n),
                 use.names = FALSE)

# T-47s are slightly more popular (70%) on Hoth, X-34s are slightly (80%)
# more popular on Tatooine:
const_vehicles <- c('a.) X-34', 'b.) T-47')
generate_vehicles <- runif(sum(const_n))
vehicle <-
    case_when(planet == const_planets[1] ~
                  case_when(generate_vehicles < .8 ~ const_vehicles[1],
                            TRUE ~ const_vehicles[2]),
              TRUE ~ case_when(generate_vehicles < .7 ~ const_vehicles[2],
                               TRUE ~ const_vehicles[1]))

# Both planets have an equal mix of Youthful (30%),
# Experienced (69%) , and Jedi (1% of people)
const_pilots <- c('a.) Experienced',
                  'b.) Youthful',
                  'c.) Jedi')
probs_pilots <- c(.69, .3, .1)

generate_pilots <- runif(sum(const_n))
pilot <- sample(const_pilots, size = sum(const_n), prob = probs_pilots,
                replace = TRUE)

# These are the true 'rating tables' -- the actual risk differences (linear
# predictor scale) for each risk in each environment

factors_vehicle <-
    expand.grid(planet = const_planets,
                vehicle = const_vehicles,
                stringsAsFactors = FALSE) %>%

    # The T-47 is way less dangerous on Hoth, but doesn't
    # work that great on Tatooine.  The opposite is true of the
    # X-34.
    bind_cols(beta_vehicle = c(0, 0.8, 0.2, -0.5))

factors_pilot <-
    expand.grid(pilot = const_pilots,
                planet = const_planets,
                stringsAsFactors = FALSE) %>%

    # Jedi are, unsurprisingly, excellent pilots everywhere (except
    # when they are getting into trouble, that is).
    #
    # On Hoth, however, Youthful pilots are a lot less risky than
    # on Tatooine, because they don't get to be the harsh
    # environment without significant training
    bind_cols(beta_pilot = c(0, 1.5, -5,
                             0, 0.2, -5))

factors_planet <-
    data.frame(planet = const_planets,

               # On average, Hoth is *slightly* more dangerous than
               # Tatooine
               beta_planet = c(0, 0.15))

## Next, we're going to give each pilot their own risk factor -- one that is
## completely unique to them, that is just their inherent level of piloting
## skill given their experience.  The differences are going to be wider on
## Hoth.

const_pilot_mean <- c(-2.3, -2.3)
const_pilot_sd <- c(0.05, 0.15)

betas_pilot_inherent <-
    unlist(mapply(rnorm,
                  n = const_n,
                  mean = const_pilot_mean,
                  sd = const_pilot_sd),
           use.names = FALSE)

## Now generate some unique IDs for each pilot, and a random number of terms
## for each pilot, averaging three terms, capping at 5
pilot_id <- str_pad(1:sum(const_n),
                    str_length(as.character(sum(const_n))),
                    pad = 0)

n_terms <- pmin(pmax(rpois(sum(const_n), 3), 1), 5)
           # in reality this is not exactly poisson,
           # but maybe close if survival is exponetial?
           # ¯\_(ツ)_/¯

## Bring all of the pilot-level data together
sample_data_pilots <-
    data.frame(pilot_id,
               n_terms,
               planet,
               vehicle,
               pilot,
               betas_pilot_inherent)

## Replicate for all terms (note that if these were real data, we'd have to
## think about exposure here, but in this fake data every row is 1 standard
## galactic year)
sample_data_terms <-
    sample_data_pilots[rep(seq(nrow(sample_data_pilots)),
                           sample_data_pilots$n_terms),] %>%
    left_join(factors_pilot) %>%
    left_join(factors_planet) %>%
    left_join(factors_vehicle) %>%
    mutate(eta = betas_pilot_inherent +
               beta_pilot +
               beta_planet +
               beta_vehicle,

           total_claims = rpois(n(), exp(eta))) %>%
    select(pilot_id, planet, vehicle, pilot, total_claims) %>%
    mutate_if(is.character, factor)

## split train and test sets
train_pilots <- sample_frac(data.frame(pilot_id = pilot_id), 0.5)

train <- sample_data_terms %>% inner_join(train_pilots)
test <- sample_data_terms %>% anti_join(train_pilots)

## Write out data
saveRDS(train, './data/simulated_landspeeder_data_train.rds')
saveRDS(test, './data/simulated_landspeeder_data_test.rds')
