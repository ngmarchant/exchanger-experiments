library(BDD)
library(comparator)
library(tidyverse)
library(exchanger)
source("run_sadinle.R")

expt_name <- paste0("RLdata10000_sadinle_", gsub("[ :]", "_", date()))

setwd("./datasets")
source("load_RLdata10000.R")
setwd("../")

rec_ids <- records$rec_id

scoring_fns <- list(
  fname_c1 = Levenshtein(normalize = TRUE),
  lname_c1 = Levenshtein(normalize = TRUE),
  by = BinaryComp(),
  bm = BinaryComp(),
  bd = BinaryComp()
)

scoring_breaks <- list(
  fname_c1 = c(-Inf,.1,.2,.3,.5,Inf),
  lname_c1 = c(-Inf,.1,.2,.3,.5,Inf),
  by = c(-Inf,0,Inf),
  bm = c(-Inf,0,Inf),
  bd = c(-Inf,0,Inf)
)

system.time(
pairs <- records %>% 
  pairs_fuzzyblock('fname_c1', scoring_fns$fname_c1, 0.3, id_col = 'rec_id') %>%
  compute_scores(records, scoring_fns, id_col = 'rec_id') %>% 
  discretize_scores(scoring_breaks)
)

message("Number of pairs:", nrow(pairs))

lambda <- list(
  fname_c1 = rep(0.95, 4),
  lname_c1 = rep(0.95, 4),
  by = rep(0.95),
  bm = rep(0.95),
  bd = rep(0.95)
)

criterion1 <- (pairs$fname_c1 < 3) & (pairs$lname_c1 < 3)
criterion1[is.na(criterion1)] <- TRUE # don't drop if either attribute is missing
criterion2 <- TRUE
#criterion2 <- pairs$by < 2
#criterion2[is.na(criterion2)] <- FALSE # don't drop if attribute is missing

pairs[['candidate']] <- (criterion1 & criterion2) # FALSE for pairs that are definite non-matches a-priori

message("Number of candidate matching pairs:", sum(pairs$candidate))

model <- BDD(pairs, lambda, id_cols = c("rec_id.x", "rec_id.y"), candidate_col = "candidate")
run_sadinle(expt_name, model, rec_ids, true_membership, n_samples = 10000, 
            burnin_interval = 100000)
