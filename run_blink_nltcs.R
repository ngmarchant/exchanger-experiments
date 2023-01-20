library(exchanger)
library(tidyverse)
source("util.R")
source("run_ours.R")

setwd("./datasets")
source("load_nltcs.R")
setwd("../")

clust_prior <- GeneralizedCouponRP(n_records, Inf)

distort_prior <- BetaRV(0.001 * n_records, 0.1 * n_records)

attr_params <- c(
  "SEX" = CategoricalAttribute(distort_prior, 
                               exclude_entity_value = FALSE),
  "DOB_DAY" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                   exclude_entity_value = FALSE),
  "DOB_MONTH" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                     exclude_entity_value = FALSE),
  "DOB_YEAR" = CategoricalAttribute(distort_prob_prior = distort_prior, 
                                    exclude_entity_value = FALSE),
  "REGOFF" = CategoricalAttribute(distort_prior, 
                                  exclude_entity_value = FALSE)
)

model <- exchanger(records, attr_params, clust_prior)

expt_name <- paste0("nltcs_blink_", gsub("[ :]", "_", date()))
run_ours(expt_name, model, true_membership, n_samples = 10000, burnin_interval = 100000)
