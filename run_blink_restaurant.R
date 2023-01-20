library(exchanger)
library(tidyverse)
source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_restaurant.R")
setwd("../")

clust_prior <- GeneralizedCouponRP(n_records, Inf)

distort_prior <- BetaRV(0.001 * n_records, 0.1 * n_records)

dist_1 <- function(x, y) {
  x <- strsplit(x, '\\s+')
  y <- strsplit(y, '\\s+')
  FuzzyTokenSet(Abbreviation(), deletion = 0.3, insertion = 0.5)(x, y)
}

attr_params <- c(
  "name" = Attribute(transform_dist_fn(dist_1, 3.0, scaling_factor = 10.0), 
                     distort_prob_prior = distort_prior, 
                     exclude_entity_value = FALSE),
  "addr" = Attribute(transform_dist_fn(dist_1, 3.0, scaling_factor = 10.0), 
                     distort_prob_prior = distort_prior, 
                     exclude_entity_value = FALSE),
  "city" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                exclude_entity_value = FALSE),
  "type" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                exclude_entity_value = FALSE)
)

model <- exchanger(records, attr_params, clust_prior)

expt_name <- paste0("restaurant_blink_", gsub("[ :]", "_", date()))
run_ours(expt_name, model, true_membership, n_samples = 10000, burnin_interval = 100000)
