library(exchanger)
library(tidyverse)
source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_restaurant.R")
setwd("../")

clust_prior <- GeneralizedCouponRP(ShiftedNegBinomRV(2, 1/10000), GammaRV(1, 1/100))

distort_prior <- BetaRV(1, 4)

dist_1 <- function(x, y) {
  x <- strsplit(x, '\\s+')
  y <- strsplit(y, '\\s+')
  FuzzyTokenSet(Abbreviation(), deletion = 0.3, insertion = 0.5)(x, y)
}

attr_params <- c(
  "name" = Attribute(transform_dist_fn(dist_1, 3.0, scaling_factor = 10.0), 
                     distort_prob_prior = distort_prior, 
                     distort_dist_prior = DirichletProcess(1),
                     entity_dist_prior = DirichletRV(1.0)),
  "addr" = Attribute(transform_dist_fn(dist_1, 3.0, scaling_factor = 10.0), 
                     distort_prob_prior = distort_prior, 
                     #distort_dist_prior = DirichletProcess(1),
                     entity_dist_prior = DirichletRV(1.0)),
  "city" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                #distort_dist_prior = DirichletProcess(1),
                                entity_dist_prior = DirichletRV(1.0)),
  "type" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                #distort_dist_prior = DirichletProcess(1), 
                                entity_dist_prior = DirichletRV(1.0))
)

model <- exchanger(records, attr_params, clust_prior)

# Run for GenCoupon
expt_name <- paste0("restaurant_ours_coupon_", gsub("[ :]", "_", date()))
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)

# Run for PY
expt_name <- paste0("restaurant_ours_py_", gsub("[ :]", "_", date()))
model@clust_prior <- PitmanYorRP(GammaRV(1, 1/100), BetaRV(1, 1))
model@clust_params <- PitmanYorRP(mean(GammaRV(1, 1/100)), mean(BetaRV(1, 1)))
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)

# Run for Ewens
expt_name <- paste0("restaurant_ours_ewens_", gsub("[ :]", "_", date()))
model@clust_prior <- EwensRP(GammaRV(1, 1/100))
model@clust_params <- EwensRP(mean(GammaRV(1, 1/100)))
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)

# Run for Blink Coupon
expt_name <- paste0("restaurant_ours_blinkcoupon_", gsub("[ :]", "_", date()))
model@clust_prior <- GeneralizedCouponRP(nrow(records), Inf)
model@clust_params <- model@clust_prior
run_ours(expt_name, model, true_membership, n_samples = 9000, burnin_interval = 10000)
