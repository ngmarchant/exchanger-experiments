library(comparator)
library(tidyverse)
library(BDD)
source("run_sadinle.R")

expt_name <- paste0("nltcs_sadinle_", gsub("[ :]", "_", date()))

setwd("./datasets")
source("load_nltcs.R")
setwd("../")

rec_ids <- seq_len(n_records)
records$ID <- rec_ids

scoring_fns <- list(
  SEX = BinaryComp(),
  DOB_DAY = BinaryComp(),
  DOB_MONTH = BinaryComp(),
  DOB_YEAR = BinaryComp(),
  REGOFF = BinaryComp()
)

scoring_breaks <- list(
  SEX = c(-Inf, 0, Inf),
  DOB_DAY = c(-Inf, 0, Inf),
  DOB_MONTH = c(-Inf, 0, Inf),
  DOB_YEAR = c(-Inf, 0, Inf),
  REGOFF = c(-Inf, 0, Inf)
)

system.time(
pairs <- records %>% 
  pairs_hamming(attr_cols = names(scoring_fns), id_col = 'ID', dist_cutoff = 2) %>%
  compute_scores(records, scoring_fns, id_col = 'ID') %>% 
  discretize_scores(scoring_breaks)
)

message("Number of pairs:", nrow(pairs))

# this one is the one I'd use
lambda <- list(
  SEX = c(0.95),
  DOB_DAY = c(0.95),
  DOB_MONTH = c(0.95),
  DOB_YEAR = c(0.95),
  REGOFF = c(0.95)
)

model <- BDD(pairs, lambda, id_cols = c("ID.x", "ID.y"))
run_sadinle(expt_name, model, rec_ids, true_membership, n_samples = 10000, 
            burnin_interval = 100000)
