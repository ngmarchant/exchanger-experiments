library(furrr)
plan(multisession(workers=4))

library(exchanger)
library(comparator)
library(tidyverse)

source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_RLdata10000.R")
setwd("../")

snbinom_var <- n_records^2
snbinom_size <- (n_records - 1)^2 / (snbinom_var - n_records + 1)
snbinom_prob <- (n_records - 1) / snbinom_var

expt_configs = list(
  list(name = "coupon", clust_prior = GeneralizedCouponRP(ShiftedNegBinomRV(snbinom_size, snbinom_prob), GammaRV(1, 1/100))),
  list(name = "py", clust_prior = PitmanYorRP(GammaRV(1, 1/100), BetaRV(1, 1))),
  list(name = "ewens", clust_prior = EwensRP(GammaRV(1, 1/100))),
  list(name = "blinkcoupon", clust_prior = GeneralizedCouponRP(nrow(records), Inf))
)

future_map(expt_configs, function(e) {
  distort_prior <- BetaRV(1, 4)
  
  attr_params <- c(
    "fname_c1" = Attribute(transform_dist_fn(Levenshtein(normalize = TRUE), 3.0, scaling_factor = 10.0), 
                           distort_prior, 
                           exclude_entity_value = FALSE),
    "lname_c1" = Attribute(transform_dist_fn(Levenshtein(normalize = TRUE), 3.0, scaling_factor = 10.0), 
                           distort_prior, 
                           exclude_entity_value = FALSE),
    "by" = CategoricalAttribute(distort_prior, 
                                exclude_entity_value = FALSE),
    "bm" = CategoricalAttribute(distort_prior, 
                                exclude_entity_value = FALSE),
    "bd" = CategoricalAttribute(distort_prior, 
                                exclude_entity_value = FALSE)
  )
  
  model <- exchanger(records, attr_params, e$clust_prior)
  expt_name <- paste0("RLdata10000_ours_blinkdist_", e$name, "_", gsub("[ :]", "_", date()))
  run_ours(expt_name, model, true_membership, n_samples = 10000, burnin_interval = 100000)
}, .options = furrr_options(packages=c("comparator", "exchanger", "clevr")))
