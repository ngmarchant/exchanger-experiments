library(exchanger)
library(comparator)
library(tidyverse)
source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_RLdata10000.R")
setwd("../")

clust_prior <- GeneralizedCouponRP(n_records, Inf)

distort_prior <- BetaRV(0.001 * n_records, 0.1 * n_records)

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

expt_name <- paste0("RLdata10000_blink_", gsub("[ :]", "_", date()))
run_ours(expt_name, model, true_membership, n_samples = 10000, burnin_interval = 100000)
