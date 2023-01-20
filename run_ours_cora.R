library(furrr)
plan(multisession(workers=4))

library(exchanger)
library(comparator)
library(tidyverse)

source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_cora.R")
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
  
  dist_2 <- function(x, y) {
    x <- strsplit(x, '\\s+')
    y <- strsplit(y, '\\s+')
    FuzzyTokenSet(Abbreviation(), deletion=0.5)(x, y)
  }
  
  attr_params <- c(
    "authors" = Attribute(transform_dist_fn(dist_2, 3.0, scaling_factor = 10.0),
                          distort_prob_prior = distort_prior,
                          distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)), 
                          entity_dist_prior = DirichletRV(1.0)),
    "title" = Attribute(transform_dist_fn(dist_2, 3.0, scaling_factor = 10.0), 
                        distort_prob_prior = distort_prior, 
                        distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)), 
                        entity_dist_prior = DirichletRV(1.0)),
    "venue" = Attribute(transform_dist_fn(dist_2, 5.0, scaling_factor = 10.0),
                        distort_prob_prior = distort_prior,
                        distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)), 
                        entity_dist_prior = DirichletRV(1.0)),
    "year" = Attribute(transform_dist_fn(Levenshtein(normalize=TRUE), 5.0, scaling_factor = 10.0), 
                       distort_prob_prior = distort_prior, 
                       distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)), 
                       entity_dist_prior = DirichletRV(1.0))
  )
  
  model <- exchanger(records, attr_params, e$clust_prior)
  expt_name <- paste0("cora_ours_", e$name, "_", gsub("[ :]", "_", date()))
  run_ours(expt_name, model, true_membership, n_samples = 10000, burnin_interval = 100000)
}, .options = furrr_options(packages=c("comparator", "exchanger", "clevr")))
