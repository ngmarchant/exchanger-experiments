library(furrr)
library(purrr)
plan(multisession(workers=24))

library(comparator)
library(tidyverse)
library(BDD)
source("run_sadinle.R")

setwd("./datasets")
source("load_synthdata.R")
setwd("../")

expt_configs <- transpose(expand.grid(link_conf_mu = c(0.1, 1, 8, 100), dist_conf = 0:1, seed = 0, exp_num_recs = 1000))

future_map(expt_configs, function(e) {
  expt_name <- paste0("synthdata_link-conf-mu-", e$link_conf_mu, "_dist-conf-", 
                      e$dist_conf, "_seed-", e$seed, "_sadinle_", 
                      gsub("[ :]", "_", date()))
  
  sel_idx <- which(file.synthdata$link_conf_mu == e$link_conf_mu & file.synthdata$dist_conf == e$dist_conf & file.synthdata$seed == e$seed & file.synthdata$exp_num_recs == e$exp_num_recs)
  
  records <- synthdata[sel_idx,]
  n_records <- nrow(records)
  records$rec_id <- seq_len(n_records)
  rec_ids <- records$rec_id
  true_membership <- identity.synthdata[sel_idx]
  
  scoring_fns <- list(
    first_name = Levenshtein(normalize=TRUE),
    last_name = Levenshtein(normalize=TRUE),
    zipcode = BinaryComp(),
    birth_year = BinaryComp(),
    birth_month = BinaryComp(),
    birth_day = BinaryComp(),
    gender = BinaryComp()
  )
  
  scoring_breaks <- list(
    first_name = c(-Inf,.1,.2,.3,.5,Inf),
    last_name = c(-Inf,.1,.2,.3,.5,Inf),
    zipcode = c(-Inf,0,Inf),
    birth_year = c(-Inf,0,Inf),
    birth_month = c(-Inf,0,Inf),
    birth_day = c(-Inf,0,Inf),
    gender = c(-Inf,0,Inf)
  )
  
  system.time(
    pairs <- records %>% 
      pairs_fuzzyblock('last_name', scoring_fns$last_name, 0.5, id_col = 'rec_id') %>%
      compute_scores(records, scoring_fns, id_col = 'rec_id') %>% 
      discretize_scores(scoring_breaks)
  )
  
  lambda <- list(
    first_name = rep(0.95, 4),
    last_name = rep(0.95, 4),
    zipcode = c(0.95),
    birth_year = rep(0.95),
    birth_month = rep(0.95),
    birth_day = rep(0.95),
    gender = rep(0.95)
  )
  
  criterion1 <- (pairs$first_name < 4) & (pairs$last_name < 4)
  criterion1[is.na(criterion1)] <- TRUE # don't drop if any of above attributes are missing
  
  pairs[['candidate']] <- criterion1 # FALSE for pairs that are definite non-matches a-priori
  
  model <- BDD(pairs, lambda, id_cols = c("rec_id.x", "rec_id.y"), candidate_col = "candidate")
  run_sadinle(expt_name, model, rec_ids, true_membership, n_samples = 10000, 
              burnin_interval = 100000)
}, .options = furrr_options(packages=c("comparator", "BDD", "exchanger", "clevr")))



