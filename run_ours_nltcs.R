library(exchanger)
library(tidyverse)
source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_nltcs.R")
setwd("../")

clust_prior <- GeneralizedCouponRP(ShiftedNegBinomRV(2, 1/10000), GammaRV(1, 1/100))

distort_prior <- BetaRV(1, 4)

attr_params <- c(
  "SEX" = CategoricalAttribute(distort_prior, 
                               entity_dist_prior = DirichletRV(1.0)),
  "DOB_DAY" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                   entity_dist_prior = DirichletRV(1.0)),
  "DOB_MONTH" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                     entity_dist_prior = DirichletRV(1.0)),
  "DOB_YEAR" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                    entity_dist_prior = DirichletRV(1.0)),
  "REGOFF" = CategoricalAttribute(distort_prior, 
                                  distort_dist_prior = DirichletProcess(1), 
                                  entity_dist_prior = DirichletRV(1.0))
)

model <- exchanger(records, attr_params, clust_prior)

# Run for GenCoupon
expt_name <- paste0("nltcs_ours_coupon_", gsub("[ :]", "_", date()))
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)

# Run for PY
expt_name <- paste0("nltcs_ours_py_", gsub("[ :]", "_", date()))
model@clust_prior <- PitmanYorRP(GammaRV(1, 1/100), BetaRV(1, 1))
model@clust_params <- PitmanYorRP(mean(GammaRV(1, 1/100)), mean(BetaRV(1, 1)))
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)

# Run for Ewens
expt_name <- paste0("nltcs_ours_ewens_", gsub("[ :]", "_", date()))
model@clust_prior <- EwensRP(GammaRV(1, 1/100))
model@clust_params <- EwensRP(mean(GammaRV(1, 1/100)))
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)

# Run for Blink Coupon
expt_name <- paste0("nltcs_ours_blinkcoupon_", gsub("[ :]", "_", date()))
model@clust_prior <- GeneralizedCouponRP(nrow(records), Inf)
model@clust_params <- model@clust_prior
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)
