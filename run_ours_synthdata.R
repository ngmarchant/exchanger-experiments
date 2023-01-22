library(furrr)
library(purrr)
plan(multisession(workers=24))

library(exchanger)
library(comparator)
library(tidyverse)

source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_synthdata.R")
setwd("../")

expt_configs <- transpose(expand.grid(link_conf_mu = c(0.1, 1, 8, 100), dist_conf = 0:1, seed = 0, exp_num_recs = 1000))

future_map(expt_configs, function(e) {
  expt_name <- paste0("synthdata_link-conf-mu-", e$link_conf_mu, "_dist-conf-", 
                      e$dist_conf, "_seed-", e$seed, "_ours_coupon_", 
                      gsub("[ :]", "_", date()))
  sel_idx <- which(file.synthdata$link_conf_mu == e$link_conf_mu & file.synthdata$dist_conf == e$dist_conf & file.synthdata$seed == e$seed & file.synthdata$exp_num_recs == e$exp_num_recs)
  records <- synthdata[sel_idx,]
  true_membership <- identity.synthdata[sel_idx]
  
  n_records <- nrow(records)
  snbinom_var <- n_records^2
  snbinom_size <- (n_records - 1)^2 / (snbinom_var - n_records + 1)
  snbinom_prob <- (n_records - 1) / snbinom_var
  clust_prior <- GeneralizedCouponRP(ShiftedNegBinomRV(snbinom_size, snbinom_prob), GammaRV(1, 1/100))

  distort_prior <- BetaRV(1, 4)

  attr_params <- c(
    first_name = Attribute(
      transform_dist_fn(Levenshtein(normalize = TRUE), 5.0, scaling_factor = 10.0), 
      distort_prob_prior = distort_prior, 
      distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
      entity_dist_prior = DirichletRV(1.0)
    ),
    last_name = Attribute(
      transform_dist_fn(Levenshtein(normalize = TRUE), 5.0, scaling_factor = 10.0), 
      distort_prob_prior = distort_prior, 
      distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
      entity_dist_prior = DirichletRV(1.0)
    ),
    zipcode = CategoricalAttribute(
      distort_prob_prior = distort_prior,
      distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
      entity_dist_prior = DirichletRV(1.0)
    ),
    birth_month = CategoricalAttribute(
      distort_prob_prior = distort_prior, 
      distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
      entity_dist_prior = DirichletRV(1.0)
    ),
    birth_day = CategoricalAttribute(
      distort_prob_prior = distort_prior, 
      distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
      entity_dist_prior = DirichletRV(1.0)
    ),
    birth_year = CategoricalAttribute(
      distort_prob_prior = distort_prior, 
      distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
      entity_dist_prior = DirichletRV(1.0)
    ),
    gender = CategoricalAttribute(
      distort_prob_prior = distort_prior, 
      distort_dist_prior = DirichletProcess(GammaRV(2, 1e-4)),
      entity_dist_prior = DirichletRV(1.0)
    )
  )

  model <- exchanger(records, attr_params, clust_prior)

  # Run for Coupon
  run_ours(expt_name, model, true_membership, n_samples = 10000, burnin_interval = 100000)
}, .options = furrr_options(packages=c("comparator", "exchanger", "clevr")))
