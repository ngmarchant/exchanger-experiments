library(exchanger)
library(comparator)
library(tidyverse)

source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_RLdata10000.R")
setwd("../")

clust_prior <- GeneralizedCouponRP(ShiftedNegBinomRV(2, 1/10000), GammaRV(1, 1/100))

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

model <- exchanger(records, attr_params, clust_prior, rec_id_colname='rec_id')

# Run for GenCoupon
expt_name <- paste0("RLdata10000_ours_blinkdist_coupon_", gsub("[ :]", "_", date()))
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)

# Run for PY
expt_name <- paste0("RLdata10000_ours_blinkdist_py_", gsub("[ :]", "_", date()))
model@clust_prior <- PitmanYorRP(GammaRV(1, 1/100), BetaRV(1, 1))
model@clust_params <- PitmanYorRP(mean(GammaRV(1, 1/100)), mean(BetaRV(1, 1)))
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)

# Run for Ewens
expt_name <- paste0("RLdata10000_ours_blinkdist_ewens_", gsub("[ :]", "_", date()))
model@clust_prior <- EwensRP(GammaRV(1, 1/100))
model@clust_params <- EwensRP(mean(GammaRV(1, 1/100)))
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)

# Run for Blink Coupon
expt_name <- paste0("RLdata10000_ours_blinkdist_blinkcoupon_", gsub("[ :]", "_", date()))
model@clust_prior <- GeneralizedCouponRP(nrow(records), Inf)
model@clust_params <- model@clust_prior
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)
