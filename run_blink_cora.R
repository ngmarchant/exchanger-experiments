library(exchanger)
library(tidyverse)
source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_cora.R")
setwd("../")

clust_prior <- GeneralizedCouponRP(n_records, Inf)

distort_prior <- BetaRV(0.001 * n_records, 0.1 * n_records)

dist_2 <- function(x, y) {
  x <- strsplit(x, '\\s+')
  y <- strsplit(y, '\\s+')
  FuzzyTokenSet(Abbreviation(), deletion=0.5)(x, y)
}

attr_params <- c(
  "authors" = Attribute(transform_dist_fn(dist_2, 3.0, scaling_factor = 10.0), 
                        distort_prob_prior = distort_prior, 
                        exclude_entity_value = FALSE),
  "title" = Attribute(transform_dist_fn(dist_2, 3.0, scaling_factor = 10.0), 
                      distort_prob_prior = distort_prior, 
                      exclude_entity_value = FALSE),
  "venue" = Attribute(transform_dist_fn(dist_2, 5.0, scaling_factor = 10.0), 
                      distort_prob_prior = distort_prior, 
                      exclude_entity_value = FALSE),
  "year" = Attribute(transform_dist_fn(Levenshtein(normalize=TRUE), 5.0, scaling_factor = 10.0), 
                     distort_prob_prior = distort_prior, 
                     exclude_entity_value = FALSE)
)

model <- exchanger(records, attr_params, clust_prior)

expt_name <- paste0("cora_blink_", gsub("[ :]", "_", date()))
run_ours(expt_name, model, true_membership, n_samples = 10000, burnin_interval = 100000)
